# 第五章：COBOL 调用 C 函数

## 5.1 为什么需要调用 C？

COBOL 是一门专注于业务逻辑的语言，但有些功能它无法直接实现：
- 访问操作系统 API
- 使用第三方库（如 SQLite）
- 高性能计算
- 网络编程

通过调用 C 函数，COBOL 可以获得这些能力。

## 5.2 CALL 语句基础

COBOL 使用 `CALL` 语句调用外部程序或函数：

```cobol
       CALL "function_name" USING param1 param2 ...
                            RETURNING result
```

### 5.2.1 简单示例

C 函数：
```c
// mylib.c
int add_numbers(int a, int b) {
    return a + b;
}
```

COBOL 调用：
```cobol
       WORKING-STORAGE SECTION.
       01 WS-A        PIC S9(9) COMP-5 VALUE 10.
       01 WS-B        PIC S9(9) COMP-5 VALUE 20.
       01 WS-RESULT   PIC S9(9) COMP-5 VALUE 0.
       
       PROCEDURE DIVISION.
           CALL "add_numbers" USING BY VALUE WS-A
                                    BY VALUE WS-B
                              RETURNING WS-RESULT
           DISPLAY "Result: " WS-RESULT.
```

## 5.3 参数传递方式

### 5.3.1 BY REFERENCE（默认）

传递变量的地址，C 函数可以修改变量的值：

```cobol
       CALL "modify_value" USING BY REFERENCE WS-VALUE
```

对应的 C 函数：
```c
void modify_value(int *value) {
    *value = *value * 2;
}
```

### 5.3.2 BY VALUE

传递变量的值（副本），C 函数无法修改原变量：

```cobol
       CALL "process_value" USING BY VALUE WS-VALUE
```

对应的 C 函数：
```c
void process_value(int value) {
    // value 是副本，修改不影响原变量
}
```

### 5.3.3 BY CONTENT

类似 BY VALUE，但用于字符串等复杂类型：

```cobol
       CALL "print_string" USING BY CONTENT WS-MESSAGE
```

## 5.4 数据类型映射

COBOL 和 C 的数据类型必须正确对应：

| COBOL 类型 | C 类型 | 说明 |
|-----------|--------|------|
| `PIC S9(9) COMP-5` | `int` | 32位有符号整数 |
| `PIC S9(18) COMP-5` | `long` | 64位有符号整数 |
| `PIC X(n)` | `char *` | 字符串（注意：COBOL 字符串不以 null 结尾！） |
| `POINTER` | `void *` | 指针 |

### 5.4.1 整数类型

```cobol
      *> 与 C 的 int 兼容
       01 WS-INT          PIC S9(9) COMP-5.
       
      *> 与 C 的 long 兼容
       01 WS-LONG         PIC S9(18) COMP-5.
```

**重要：** 必须使用 `COMP-5`，不能用 `COMP`！

`COMP` 的字节序可能与 C 不同，而 `COMP-5` 保证使用原生字节序。

### 5.4.2 字符串类型

COBOL 字符串是固定长度的，用空格填充，**没有 null 终止符**：

```cobol
       01 WS-NAME    PIC X(20) VALUE "John".
      *> 实际存储: "John                " (16个空格)
```

C 字符串以 null 结尾：
```c
char name[] = "John";  // 实际存储: "John\0"
```

#### 从 COBOL 传字符串到 C

COBOL 传给 C 的字符串会带有尾部空格，需要在 C 端 trim：

```c
// 去除 COBOL 字符串尾部空格
static char* trim_string(const char *str, char *buf, size_t bufsize) {
    if (!str || !buf) {
        if (buf) buf[0] = '\0';
        return buf;
    }
    strncpy(buf, str, bufsize - 1);
    buf[bufsize - 1] = '\0';
    
    // 去除尾部空格
    size_t len = strlen(buf);
    while (len > 0 && buf[len - 1] == ' ') {
        buf[--len] = '\0';
    }
    return buf;
}

// 使用示例
int db_open(const char *db_path) {
    char path_buf[256];
    trim_string(db_path, path_buf, sizeof(path_buf));
    // 现在 path_buf 是干净的 C 字符串
    return sqlite3_open(path_buf, &db);
}
```

#### 从 C 写字符串到 COBOL 变量（重要！）

当 C 函数需要把字符串写回 COBOL 变量时，**不能使用 null 终止符**！

❌ **错误做法：**
```c
void get_name(char *name) {
    strcpy(name, "John");
    // name 现在是 "John\0"
    // COBOL 的 FUNCTION TRIM 会在 \0 处截断，导致比较失败！
}
```

✅ **正确做法：**
```c
void get_name(char *name) {
    // 假设 COBOL 端是 PIC X(20)
    memset(name, ' ', 20);      // 先用空格填充整个缓冲区
    memcpy(name, "John", 4);    // 复制内容，不包含 \0
    // name 现在是 "John                "（COBOL 格式）
}
```

#### 实际案例

在本项目中，`rpt_category_next` 函数需要把数据库查询结果写回 COBOL 变量：

```c
int rpt_category_next(char *category, char *type, long *total) {
    // ... 从数据库读取 ...
    const char *col_type = (const char *)sqlite3_column_text(stmt, 1);
    
    // 写入 COBOL 变量 - 用空格填充，不加 null
    memset(type, ' ', 8);  // COBOL 端是 PIC X(8)
    if (col_type) {
        size_t len = strlen(col_type);
        if (len > 8) len = 8;
        memcpy(type, col_type, len);
    }
    // type 现在是 "EXPENSE " 或 "INCOME  "（COBOL 格式）
    
    return RC_SUCCESS;
}
```

COBOL 端可以正常使用 `FUNCTION TRIM` 比较：
```cobol
       IF FUNCTION TRIM(WS-RPT-TYPE) = "EXPENSE"
           DISPLAY "This is an expense"
       END-IF
```

#### 总结：COBOL-C 字符串传递规则

| 方向 | 处理方式 |
|------|----------|
| COBOL → C | C 端用 `trim_string()` 去除尾部空格 |
| C → COBOL | C 端用 `memset` 填充空格 + `memcpy` 复制内容，**不加 null** |

## 5.5 返回值

使用 `RETURNING` 子句获取 C 函数的返回值：

```cobol
       01 WS-RC    PIC S9(9) COMP-5.
       
       CALL "some_function" USING ...
                            RETURNING WS-RC
       
       IF WS-RC NOT = 0
           DISPLAY "Error occurred"
       END-IF
```

## 5.6 实际案例：SQLite 包装函数

### 5.6.1 C 端实现

```c
// sqlite_wrapper.c
#include <sqlite3.h>
#include <string.h>

static sqlite3 *db = NULL;

// 返回码定义
#define RC_SUCCESS      0
#define RC_PARAM_ERROR  1
#define RC_DB_ERROR     2
#define RC_NOT_FOUND    3
#define RC_IO_ERROR     4

// 去除 COBOL 字符串尾部空格
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

// 打开数据库
int db_open(const char *db_path) {
    if (db != NULL) {
        return RC_SUCCESS;  // 已打开
    }
    
    // trim COBOL 传来的路径
    char path_buf[256];
    trim_string(db_path, path_buf, sizeof(path_buf));
    
    int rc = sqlite3_open(path_buf, &db);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        db = NULL;
        return RC_DB_ERROR;
    }
    return RC_SUCCESS;
}

// 关闭数据库
int db_close(void) {
    if (db) {
        sqlite3_close(db);
        db = NULL;
    }
    return RC_SUCCESS;
}

// 初始化数据库表结构
int db_init_schema(void) {
    if (db == NULL) return RC_DB_ERROR;
    
    const char *sql = 
        "CREATE TABLE IF NOT EXISTS transactions ("
        "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        "  tx_date TEXT NOT NULL,"
        "  tx_type TEXT NOT NULL,"
        "  category TEXT NOT NULL,"
        "  amount_cents INTEGER NOT NULL,"
        "  note TEXT"
        ");";
    
    char *err_msg = NULL;
    int rc = sqlite3_exec(db, sql, NULL, NULL, &err_msg);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", err_msg);
        sqlite3_free(err_msg);
        return RC_DB_ERROR;
    }
    return RC_SUCCESS;
}
```

### 5.6.2 COBOL 端调用

```cobol
       WORKING-STORAGE SECTION.
      *> 返回码（必须用 COMP-5 与 C 的 int 兼容）
       01 WS-RC              PIC S9(9) COMP-5 VALUE 0.
       
      *> 数据库路径（多留一个字节给 null 终止符）
       01 WS-DB-PATH         PIC X(201) VALUE SPACES.
       
       PROCEDURE DIVISION.
       CMD-INIT.
           DISPLAY "Initializing database: " 
                   FUNCTION TRIM(WS-DB-PATH)
           
      *>   打开数据库
           CALL "db_open" USING BY REFERENCE WS-DB-PATH
                          RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot open database"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   创建表结构
           CALL "db_init_schema" RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot create schema"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Database initialized successfully".
```

## 5.7 复杂参数：多个输出参数

当 C 函数需要返回多个值时，使用指针参数：

### C 端：
```c
int tx_list_next(int *id, char *date, char *type, char *category,
                 long *amount_cents, char *note) {
    // ... 从数据库读取一行 ...
    
    *id = sqlite3_column_int(stmt, 0);
    strncpy(date, (const char *)sqlite3_column_text(stmt, 1), 10);
    strncpy(type, (const char *)sqlite3_column_text(stmt, 2), 7);
    // ...
    *amount_cents = sqlite3_column_int64(stmt, 4);
    
    return RC_SUCCESS;
}
```

### COBOL 端：
```cobol
       WORKING-STORAGE SECTION.
       01 WS-LIST-ID         PIC S9(9) COMP-5 VALUE 0.
       01 WS-LIST-DATE       PIC X(11) VALUE SPACES.
       01 WS-LIST-TYPE       PIC X(8) VALUE SPACES.
       01 WS-LIST-CATEGORY   PIC X(51) VALUE SPACES.
       01 WS-LIST-AMOUNT     PIC S9(18) COMP-5 VALUE 0.
       01 WS-LIST-NOTE       PIC X(201) VALUE SPACES.
       
       PROCEDURE DIVISION.
           CALL "tx_list_next"
                USING BY REFERENCE WS-LIST-ID
                      BY REFERENCE WS-LIST-DATE
                      BY REFERENCE WS-LIST-TYPE
                      BY REFERENCE WS-LIST-CATEGORY
                      BY REFERENCE WS-LIST-AMOUNT
                      BY REFERENCE WS-LIST-NOTE
                RETURNING WS-RC
```

## 5.8 编译和链接

### 5.8.1 编译 C 代码

```bash
# 编译为目标文件
gcc -c -fPIC sqlite_wrapper.c -o sqlite_wrapper.o \
    -I/opt/homebrew/opt/sqlite/include
```

### 5.8.2 编译 COBOL 并链接

```bash
# 编译 COBOL 并链接 C 目标文件和 SQLite 库
cobc -x -free -o ledger \
    main.cbl \
    sqlite_wrapper.o \
    -L/opt/homebrew/opt/sqlite/lib -lsqlite3
```

### 5.8.3 完整的构建脚本

```bash
#!/bin/bash
# build.sh

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC_DIR="$PROJECT_ROOT/src"
BUILD_DIR="$PROJECT_ROOT/build"
BIN_DIR="$BUILD_DIR/bin"
OBJ_DIR="$BUILD_DIR/obj"

# 创建目录
mkdir -p "$BIN_DIR" "$OBJ_DIR"

# SQLite 路径（macOS Homebrew）
SQLITE_INC="-I/opt/homebrew/opt/sqlite/include"
SQLITE_LIB="-L/opt/homebrew/opt/sqlite/lib"

# 编译 C 包装函数
echo "Compiling C wrapper..."
gcc -c -fPIC $SQLITE_INC \
    "$SRC_DIR/sqlite_wrapper.c" \
    -o "$OBJ_DIR/sqlite_wrapper.o"

# 编译 COBOL 并链接
echo "Compiling COBOL..."
cobc -x -free -o "$BIN_DIR/ledger" \
    "$SRC_DIR/main.cbl" \
    "$OBJ_DIR/sqlite_wrapper.o" \
    $SQLITE_LIB -lsqlite3

echo "Build complete: $BIN_DIR/ledger"
```

## 5.9 调试技巧

### 5.9.1 在 C 端添加调试输出

```c
int db_open(const char *db_path) {
    char path_buf[256];
    trim_string(db_path, path_buf, sizeof(path_buf));
    
    // 调试输出
    fprintf(stderr, "DEBUG: Opening database: [%s]\n", path_buf);
    
    // ...
}
```

### 5.9.2 检查参数传递

如果调用不工作，检查：
1. 数据类型是否匹配（特别是 `COMP-5`）
2. 字符串是否正确处理（trim 尾部空格）
3. 参数顺序是否正确
4. BY REFERENCE vs BY VALUE 是否正确

## 5.10 练习

1. 写一个 C 函数 `get_current_time`，返回当前时间戳，从 COBOL 调用它

2. 写一个 C 函数 `string_length`，接受 COBOL 字符串，返回去除尾部空格后的长度

3. 修改 `db_open` 函数，添加更详细的错误信息

## 下一章预告

下一章我们将详细讲解 SQLite 数据库操作，包括增删改查的完整实现。
