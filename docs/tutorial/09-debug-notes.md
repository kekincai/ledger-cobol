# 第九章：调试踩坑记录

本章记录了开发这个 COBOL-C-SQLite 项目过程中遇到的实际问题和解决方案。

## 9.1 COMP vs COMP-5：整数类型的坑

### 问题现象
COBOL 调用 C 函数时，传递的整数值不正确，或者返回值乱码。

### 原因
`COMP` 类型的字节序可能与 C 不同（取决于编译器实现），而 `COMP-5` 保证使用原生字节序。

### 解决方案
所有与 C 交互的整数变量必须使用 `COMP-5`：

```cobol
      *> ❌ 错误
       01 WS-RC    PIC S9(9) COMP.
       
      *> ✅ 正确
       01 WS-RC    PIC S9(9) COMP-5.
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
type[7] = '\0';  // 这个 \0 会破坏 COBOL 字符串！
```

COBOL 的 `FUNCTION TRIM` 遇到 `\0` 会产生意外行为，导致字符串比较失败。

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

---

## 9.4 字符串长度截断问题

### 问题现象
"EXPENSE" 被截断成 "EXPENS"。

### 原因
```c
strncpy(type, col_type, 7);
type[7] = '\0';  // "EXPENSE" 有 7 个字符，这里把第 7 个字符覆盖了！
```

### 解决方案
计算正确的缓冲区大小，或者使用 memset + memcpy 方式。

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

---

## 9.6 调试技巧总结

### 9.6.1 在 C 端添加调试输出

```c
fprintf(stderr, "DEBUG: type=[%s] len=%zu\n", col_type, strlen(col_type));
```

### 9.6.2 在 COBOL 端添加调试输出

```cobol
       DISPLAY "DEBUG: type=[" WS-TYPE "]"
```

### 9.6.3 检查数据库实际内容

```bash
sqlite3 ./db/ledger.db "SELECT id, tx_type, length(tx_type), hex(tx_type) FROM transactions;"
```

`hex()` 函数可以看到字符串的实际字节，帮助发现隐藏字符。

### 9.6.4 检查文件系统

```bash
ls -la ./db/
```

如果看到文件名很长或有奇怪的空格，说明字符串没有正确 trim。

---

## 9.7 COBOL-C 字符串传递速查表

| 方向 | 问题 | 解决方案 |
|------|------|----------|
| COBOL → C | 尾部有空格 | C 端用 `trim_string()` |
| C → COBOL | null 终止符破坏比较 | 用 `memset` 空格填充 + `memcpy`，不加 `\0` |
| 两边 | 长度不匹配 | 确保 C 端缓冲区大小 >= COBOL PIC X(n) 的 n |

---

## 9.8 经验教训

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
