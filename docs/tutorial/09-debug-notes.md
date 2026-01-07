# 第九章：调试踩坑记录

本章记录了开发这个 COBOL-C-SQLite 项目过程中遇到的实际问题和解决方案。

## 9.1 COMP vs COMP-5：整数类型的坑

### 问题现象
COBOL 调用 C 函数时，传递的整数值不正确，或者返回值乱码。

### 原因
`COMP` 的存储格式由实现决定，可能是二进制，也可能有不同的对齐方式。即便都叫"二进制"，也可能因为 `sizeof`、对齐、符号位、调用约定导致互操作出错。

`COMP-5` 在常见实现（如 GnuCOBOL）中强调使用 native binary representation，更接近 C 的 `int`/`long`。

### 解决方案
所有与 C 交互的整数变量必须使用 `COMP-5`：

```cobol
      *> ❌ 错误
       01 WS-RC    PIC S9(9) COMP.
       
      *> ✅ 正确
       01 WS-RC    PIC S9(9) COMP-5.
```

### 类型对应关系

| COBOL | C | 说明 |
|-------|---|------|
| `PIC S9(9) COMP-5` | `int` / `int32_t` | 32位有符号整数 |
| `PIC S9(18) COMP-5` | `long` / `int64_t` | 64位有符号整数 |

### 如何验证
在 C 端添加断言确保大小匹配：
```c
#include <assert.h>
static_assert(sizeof(int) == 4, "int must be 4 bytes");
static_assert(sizeof(long) == 8, "long must be 8 bytes");
```

---

## 9.2 COBOL → C 字符串：尾部空格问题

### 问题现象
C 函数收到的字符串很长，包含大量空格，导致：
- 文件名带空格（如 `ledger.db                    `）
- SQL 查询失败
- 字符串比较失败

### 原因
COBOL 字符串是固定长度，用空格填充：
```cobol
       01 WS-PATH    PIC X(200) VALUE "./db/ledger.db".
      *> 实际内容: "./db/ledger.db" + 186个空格
```

### 解决方案
C 端必须 trim 所有从 COBOL 传来的字符串：

```c
static char* trim_string(const char *str, char *buf, size_t bufsize) {
    if (!str || !buf) {
        if (buf) buf[0] = '\0';
        return buf;
    }
    strncpy(buf, str, bufsize - 1);
    buf[bufsize - 1] = '\0';
    
    size_t len = strlen(buf);
    while (len > 0 && buf[len - 1] == ' ') {
        buf[--len] = '\0';
    }
    return buf;
}

int db_open(const char *db_path) {
    char path_buf[256];
    trim_string(db_path, path_buf, sizeof(path_buf));  // 必须 trim！
    return sqlite3_open(path_buf, &db);
}
```

### 如何验证
```c
fprintf(stderr, "DEBUG: path=[%s] len=%zu\n", path_buf, strlen(path_buf));
```

检查文件系统是否有带空格的文件名：
```bash
ls -la ./db/
```

---

## 9.3 C → COBOL 字符串：null 终止符的坑（最难发现！）

### 问题现象
C 函数明明返回了正确的数据，但 COBOL 端的比较总是失败：

```cobol
      *> C 返回了 "EXPENSE"，但这个比较永远不成立！
       IF FUNCTION TRIM(WS-TYPE) = "EXPENSE"
           DISPLAY "This is expense"
       END-IF
```

调试输出显示 C 端数据正确，但 COBOL 就是不匹配。

### 原因
C 代码使用了 `strncpy` + null 终止符：

```c
// ❌ 错误做法
strncpy(type, "EXPENSE", 7);
type[7] = '\0';  // 这个 \0 会破坏 COBOL 字符串比较！
```

**关键理解：** COBOL 字符串不是 C-string，`\0` 不是结束符。COBOL 字符串是定长的，里面出现 `\0` 这种非空格字符时，字符串比较会按字节逐个比较。`\0` 不等于空格，也不会被当作"字符串结束"。

`FUNCTION TRIM` 通常只移除尾部空格，不会把 `\0` 当作不存在。所以 `"EXPENSE\0"` 和 `"EXPENSE "` 比较会失败。

### 解决方案
C 写入 COBOL 变量时，用空格填充，**不加 null 终止符**：

```c
// ✅ 正确做法
memset(type, ' ', 8);           // 先用空格填充整个缓冲区
memcpy(type, "EXPENSE", 7);     // 复制内容，不包含 \0
// 结果: "EXPENSE " (7字符 + 1空格)
```

完整示例：
```c
int rpt_category_next(char *category, char *type, long *total) {
    const char *col_type = sqlite3_column_text(stmt, 1);
    
    // 写入 COBOL 变量 - 空格填充，无 null
    memset(type, ' ', 8);
    if (col_type) {
        size_t len = strlen(col_type);
        if (len > 8) len = 8;
        memcpy(type, col_type, len);
    }
    
    return RC_SUCCESS;
}
```

### 如何验证
COBOL 端：
```cobol
       DISPLAY "DEBUG: [" WS-TYPE "]"
```

SQLite 端查看实际字节：
```bash
sqlite3 ./db/ledger.db "SELECT hex(tx_type) FROM transactions;"
```

如果看到 `00`（null 的十六进制），说明数据有问题。

---

## 9.4 字符串长度与缓冲区问题

### 问题现象
字符串显示不完整，或者出现乱码/越界。

### 常见原因

**A. 拷贝长度写错**
```c
strncpy(type, "EXPENSE", 6);  // 只拷贝了 "EXPENS"，少了一个字符
```

**B. COBOL 端字段长度与 C 端不匹配**
```cobol
       01 WS-TYPE    PIC X(7).   *> 7 字节
```
```c
memset(type, ' ', 8);  // 写了 8 字节，越界！
```

**C. 忘记 COBOL 没有为 \0 预留空间**
```c
// COBOL PIC X(8) 就是 8 字节，不是 8+1
char type[9];  // C 端需要 9 字节才能存 8 字符 + \0
               // 但 COBOL 端只有 8 字节！
```

### 解决方案
1. C 端缓冲区大小必须与 COBOL `PIC X(n)` 的 `n` 完全一致
2. 不要假设可以写入 `n+1` 字节
3. 使用 `memset` + `memcpy` 而不是 `strncpy`

### 如何验证
打印实际长度：
```c
fprintf(stderr, "DEBUG: len=%zu, expected=8\n", strlen(col_type));
```

---

## 9.5 数据库文件路径问题

### 问题现象
程序在不同目录运行时找不到数据库。

### 原因
使用了相对路径 `./db/ledger.db`，但当前工作目录不是项目根目录。

### 解决方案
1. 始终从项目根目录运行程序
2. 或使用 `--db` 参数指定绝对路径
3. 或设置环境变量 `LEDGER_DB`

### 如何验证
```bash
pwd  # 确认当前目录
ls -la ./db/ledger.db  # 确认文件存在
```

---

## 9.6 NULL 值处理

### 问题现象
程序崩溃或出现乱码，特别是在处理数据库查询结果时。

### 原因
SQLite 的 `sqlite3_column_text()` 可能返回 `NULL`（当字段值为 SQL NULL 时）。

### 解决方案
始终检查 NULL：

```c
const char *col_note = sqlite3_column_text(stmt, 5);

memset(note, ' ', 201);
if (col_note) {  // 必须检查！
    size_t len = strlen(col_note);
    if (len > 200) len = 200;
    memcpy(note, col_note, len);
}
// 如果 col_note 是 NULL，note 就是全空格（COBOL 的"空值"语义）
```

---

## 9.7 BY REFERENCE vs BY VALUE

### 问题现象
传递的值不对，或者 C 函数修改了不该修改的变量。

### 原因
COBOL 默认使用 `BY REFERENCE`（传地址），但你可能在 C 端期待按值传递。

### 解决方案

| COBOL | C 函数签名 | 说明 |
|-------|-----------|------|
| `BY REFERENCE WS-VAR` | `void func(int *var)` | 传地址，C 可修改 |
| `BY VALUE WS-VAR` | `void func(int var)` | 传值，C 不能修改原变量 |

```cobol
      *> 传地址 - C 端用指针接收
       CALL "modify_value" USING BY REFERENCE WS-VALUE
       
      *> 传值 - C 端直接接收值
       CALL "process_value" USING BY VALUE WS-VALUE
```

---

## 9.8 调试技巧总结

### 9.8.1 在 C 端添加调试输出

```c
fprintf(stderr, "DEBUG: type=[%s] len=%zu\n", col_type, strlen(col_type));
```

### 9.8.2 在 COBOL 端添加调试输出

```cobol
       DISPLAY "DEBUG: [" WS-TYPE "]"
```

### 9.8.3 检查数据库实际内容

```bash
sqlite3 ./db/ledger.db "SELECT id, tx_type, length(tx_type), hex(tx_type) FROM transactions;"
```

`hex()` 函数可以看到字符串的实际字节，帮助发现隐藏的 `\0`（显示为 `00`）。

### 9.8.4 检查文件系统

```bash
ls -la ./db/
```

如果看到文件名很长或有奇怪的空格，说明字符串没有正确 trim。

---

## 9.9 COBOL-C 字符串传递速查表

| 方向 | 问题 | 解决方案 |
|------|------|----------|
| COBOL → C | 尾部有空格 | C 端用 `trim_string()` |
| C → COBOL | null 终止符破坏比较 | 用 `memset` 空格填充 + `memcpy`，不加 `\0` |
| 两边 | 长度不匹配 | 确保 C 端缓冲区大小 == COBOL PIC X(n) 的 n |
| C 端 | 数据库返回 NULL | 必须检查 `if (col != NULL)` |

---

## 9.10 COBOL↔C 互操作三条硬规则

### 规则一：数值类型
跨语言传递整数/指针大小要明确：
- 优先用 `COMP-5` + 明确 `long`/`int32`/`int64` 对应关系
- 必要时在 C 端用 `static_assert(sizeof(...))` 验证

### 规则二：字符串类型
COBOL 是定长空格填充；C 是 `\0` 终止。跨边界时：
- **COBOL → C**：必须 trim
- **C → COBOL**：必须空格填充，**禁止写入 `\0`**

### 规则三：调用约定
- 确认 COBOL 是 `BY REFERENCE` 还是 `BY VALUE`
- C 函数签名的指针层级必须匹配
- 否则表现为"值乱跳/偶发崩溃"

---

## 9.11 经验教训

1. **COBOL 和 C 的字符串模型完全不同** - 这是最容易出错的地方
2. **调试时先检查数据** - 用 `fprintf` 和 `DISPLAY` 打印实际值
3. **用 hex 查看隐藏字符** - `\0` 和空格肉眼看不出区别
4. **编译后要测试** - 改了代码不重新编译，测试的还是旧代码
5. **清理垃圾文件** - bug 可能产生带空格的文件名，要及时清理

---

## 下一步

恭喜你完成了这个 COBOL 记账工具项目！你已经学会了：
- COBOL 基础语法
- 命令行参数解析
- 调用 C 函数
- SQLite 数据库操作
- 处理 COBOL-C 互操作的各种坑

可以尝试的扩展：
- 添加更多报表类型（年度汇总、趋势分析）
- 支持多币种
- 添加预算功能
- 实现数据备份/恢复
