# 第六章：SQLite 数据库操作

## 6.1 SQLite 简介

SQLite 是一个轻量级的嵌入式数据库：
- 无需服务器，数据存储在单个文件中
- 零配置，开箱即用
- 支持标准 SQL
- 非常适合桌面应用和小型项目

## 6.2 数据库设计

### 6.2.1 表结构

```sql
-- db/schema.sql
CREATE TABLE IF NOT EXISTS transactions (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    tx_date     TEXT    NOT NULL,   -- 日期 YYYY-MM-DD
    tx_type     TEXT    NOT NULL,   -- 类型 INCOME/EXPENSE
    category    TEXT    NOT NULL,   -- 类别
    amount_cents INTEGER NOT NULL,  -- 金额（分）
    note        TEXT,               -- 备注
    created_at  TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

-- 索引加速查询
CREATE INDEX IF NOT EXISTS idx_tx_date ON transactions(tx_date);
CREATE INDEX IF NOT EXISTS idx_tx_type ON transactions(tx_type);
CREATE INDEX IF NOT EXISTS idx_tx_category ON transactions(category);
```

### 6.2.2 为什么用"分"存储金额？

浮点数有精度问题：
```
0.1 + 0.2 = 0.30000000000000004  // JavaScript/C 的浮点运算
```

用整数"分"存储可以避免这个问题：
```
10 + 20 = 30  // 10分 + 20分 = 30分 = 0.30元
```

## 6.3 C 包装函数设计

我们需要为 COBOL 提供以下功能：

```c
// sqlite_wrapper.h

// 数据库连接
int db_open(const char *db_path);
int db_close(void);
int db_init_schema(void);

// 交易 CRUD
int tx_add(const char *date, const char *type, const char *category,
           long amount_cents, const char *note);
int tx_update(int id, const char *date, const char *type, 
              const char *category, long amount_cents, const char *note);
int tx_delete(int id);
int tx_exists(int id);

// 查询（使用游标模式）
int tx_list_begin(const char *from_date, const char *to_date,
                  const char *month, const char *category,
                  const char *type, const char *keyword,
                  int limit, int offset);
int tx_list_next(int *id, char *date, char *type, char *category,
                 long *amount_cents, char *note);
int tx_list_end(void);

// 报表
int rpt_get_totals(const char *month, long *income, long *expense);
int rpt_category_begin(const char *month);
int rpt_category_next(char *category, char *type, long *total);
int rpt_category_end(void);
```

## 6.4 实现数据库连接

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>

static sqlite3 *db = NULL;

// 返回码
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

int db_open(const char *db_path) {
    if (db != NULL) {
        return RC_SUCCESS;  // 已打开
    }
    
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

int db_close(void) {
    if (db) {
        sqlite3_close(db);
        db = NULL;
    }
    return RC_SUCCESS;
}

int db_init_schema(void) {
    if (db == NULL) return RC_DB_ERROR;
    
    char *err_msg = NULL;
    
    // 创建表
    const char *sql_table = 
        "CREATE TABLE IF NOT EXISTS transactions ("
        "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        "  tx_date TEXT NOT NULL,"
        "  tx_type TEXT NOT NULL,"
        "  category TEXT NOT NULL,"
        "  amount_cents INTEGER NOT NULL,"
        "  note TEXT,"
        "  created_at TEXT NOT NULL DEFAULT (datetime('now')),"
        "  updated_at TEXT NOT NULL DEFAULT (datetime('now'))"
        ");";
    
    int rc = sqlite3_exec(db, sql_table, NULL, NULL, &err_msg);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", err_msg);
        sqlite3_free(err_msg);
        return RC_DB_ERROR;
    }
    
    // 创建索引
    sqlite3_exec(db, "CREATE INDEX IF NOT EXISTS idx_tx_date ON transactions(tx_date);", NULL, NULL, NULL);
    sqlite3_exec(db, "CREATE INDEX IF NOT EXISTS idx_tx_type ON transactions(tx_type);", NULL, NULL, NULL);
    sqlite3_exec(db, "CREATE INDEX IF NOT EXISTS idx_tx_category ON transactions(category);", NULL, NULL, NULL);
    
    return RC_SUCCESS;
}
```

## 6.5 实现新增交易

```c
int tx_add(const char *date, const char *type, const char *category,
           long amount_cents, const char *note) {
    if (db == NULL) return RC_DB_ERROR;
    
    // trim 所有输入参数
    char date_buf[16], type_buf[16], cat_buf[64], note_buf[256];
    trim_string(date, date_buf, sizeof(date_buf));
    trim_string(type, type_buf, sizeof(type_buf));
    trim_string(category, cat_buf, sizeof(cat_buf));
    trim_string(note, note_buf, sizeof(note_buf));
    
    const char *sql = 
        "INSERT INTO transactions (tx_date, tx_type, category, amount_cents, note) "
        "VALUES (?, ?, ?, ?, ?);";
    
    sqlite3_stmt *stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL prepare failed: %s\n", sqlite3_errmsg(db));
        return RC_DB_ERROR;
    }
    
    // 绑定参数（使用 SQLITE_TRANSIENT 让 SQLite 复制字符串）
    sqlite3_bind_text(stmt, 1, date_buf, -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 2, type_buf, -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 3, cat_buf, -1, SQLITE_TRANSIENT);
    sqlite3_bind_int64(stmt, 4, amount_cents);
    if (strlen(note_buf) > 0) {
        sqlite3_bind_text(stmt, 5, note_buf, -1, SQLITE_TRANSIENT);
    } else {
        sqlite3_bind_null(stmt, 5);
    }
    
    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);
    
    if (rc != SQLITE_DONE) {
        fprintf(stderr, "Insert failed: %s\n", sqlite3_errmsg(db));
        return RC_DB_ERROR;
    }
    return RC_SUCCESS;
}
```

**关键点：**
1. 使用参数化查询（`?` 占位符）防止 SQL 注入
2. 使用 `SQLITE_TRANSIENT` 让 SQLite 复制字符串，避免悬空指针
3. 空字符串绑定为 NULL

## 6.6 实现查询（游标模式）

查询可能返回多行数据，我们使用"游标模式"：
1. `tx_list_begin` - 准备查询
2. `tx_list_next` - 获取下一行（循环调用）
3. `tx_list_end` - 结束查询

```c
static sqlite3_stmt *list_stmt = NULL;

int tx_list_begin(const char *from_date, const char *to_date,
                  const char *month, const char *category,
                  const char *type, const char *keyword,
                  int limit, int offset) {
    if (db == NULL) return RC_DB_ERROR;
    
    // 清理之前的查询
    if (list_stmt) {
        sqlite3_finalize(list_stmt);
        list_stmt = NULL;
    }
    
    // trim 输入参数
    char from_buf[16], to_buf[16], month_buf[16];
    char cat_buf[64], type_buf[16], kw_buf[64];
    
    trim_string(from_date, from_buf, sizeof(from_buf));
    trim_string(to_date, to_buf, sizeof(to_buf));
    trim_string(month, month_buf, sizeof(month_buf));
    trim_string(category, cat_buf, sizeof(cat_buf));
    trim_string(type, type_buf, sizeof(type_buf));
    trim_string(keyword, kw_buf, sizeof(kw_buf));
    
    // 动态构建 SQL
    char sql[2048];
    strcpy(sql, "SELECT id, tx_date, tx_type, category, amount_cents, "
                "COALESCE(note, '') FROM transactions WHERE 1=1");
    
    if (strlen(from_buf) > 0) {
        strcat(sql, " AND tx_date >= ?");
    }
    if (strlen(to_buf) > 0) {
        strcat(sql, " AND tx_date <= ?");
    }
    if (strlen(month_buf) > 0) {
        strcat(sql, " AND substr(tx_date, 1, 7) = ?");
    }
    if (strlen(cat_buf) > 0) {
        strcat(sql, " AND category = ?");
    }
    if (strlen(type_buf) > 0) {
        strcat(sql, " AND tx_type = ?");
    }
    if (strlen(kw_buf) > 0) {
        strcat(sql, " AND (note LIKE ? OR category LIKE ?)");
    }
    
    strcat(sql, " ORDER BY tx_date DESC, id DESC LIMIT ? OFFSET ?;");
    
    // 准备语句
    if (sqlite3_prepare_v2(db, sql, -1, &list_stmt, NULL) != SQLITE_OK) {
        fprintf(stderr, "SQL prepare failed: %s\n", sqlite3_errmsg(db));
        return RC_DB_ERROR;
    }
    
    // 绑定参数
    int idx = 1;
    if (strlen(from_buf) > 0) {
        sqlite3_bind_text(list_stmt, idx++, from_buf, -1, SQLITE_TRANSIENT);
    }
    if (strlen(to_buf) > 0) {
        sqlite3_bind_text(list_stmt, idx++, to_buf, -1, SQLITE_TRANSIENT);
    }
    if (strlen(month_buf) > 0) {
        sqlite3_bind_text(list_stmt, idx++, month_buf, -1, SQLITE_TRANSIENT);
    }
    if (strlen(cat_buf) > 0) {
        sqlite3_bind_text(list_stmt, idx++, cat_buf, -1, SQLITE_TRANSIENT);
    }
    if (strlen(type_buf) > 0) {
        sqlite3_bind_text(list_stmt, idx++, type_buf, -1, SQLITE_TRANSIENT);
    }
    if (strlen(kw_buf) > 0) {
        char like_pattern[256];
        snprintf(like_pattern, sizeof(like_pattern), "%%%s%%", kw_buf);
        sqlite3_bind_text(list_stmt, idx++, like_pattern, -1, SQLITE_TRANSIENT);
        sqlite3_bind_text(list_stmt, idx++, like_pattern, -1, SQLITE_TRANSIENT);
    }
    sqlite3_bind_int(list_stmt, idx++, limit > 0 ? limit : 50);
    sqlite3_bind_int(list_stmt, idx, offset >= 0 ? offset : 0);
    
    return RC_SUCCESS;
}

int tx_list_next(int *id, char *date, char *type, char *category,
                 long *amount_cents, char *note) {
    if (list_stmt == NULL) {
        return RC_NOT_FOUND;
    }
    
    int rc = sqlite3_step(list_stmt);
    if (rc == SQLITE_ROW) {
        *id = sqlite3_column_int(list_stmt, 0);
        
        const char *col_date = (const char *)sqlite3_column_text(list_stmt, 1);
        const char *col_type = (const char *)sqlite3_column_text(list_stmt, 2);
        const char *col_cat = (const char *)sqlite3_column_text(list_stmt, 3);
        const char *col_note = (const char *)sqlite3_column_text(list_stmt, 5);
        
        if (col_date) { strncpy(date, col_date, 10); date[10] = '\0'; }
        if (col_type) { strncpy(type, col_type, 7); type[7] = '\0'; }
        if (col_cat) { strncpy(category, col_cat, 50); category[50] = '\0'; }
        *amount_cents = sqlite3_column_int64(list_stmt, 4);
        if (col_note) { strncpy(note, col_note, 200); note[200] = '\0'; }
        else { note[0] = '\0'; }
        
        return RC_SUCCESS;
    }
    return RC_NOT_FOUND;
}

int tx_list_end(void) {
    if (list_stmt) {
        sqlite3_finalize(list_stmt);
        list_stmt = NULL;
    }
    return RC_SUCCESS;
}
```

## 6.7 COBOL 端调用

```cobol
       CMD-LIST.
           CALL "db_open" USING BY REFERENCE WS-DB-PATH
                          RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot open database"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   开始查询
           CALL "tx_list_begin" 
                USING BY REFERENCE WS-ARG-FROM
                      BY REFERENCE WS-ARG-TO
                      BY REFERENCE WS-ARG-MONTH
                      BY REFERENCE WS-ARG-CATEGORY
                      BY REFERENCE WS-ARG-TYPE
                      BY REFERENCE WS-ARG-KEYWORD
                      BY VALUE WS-ARG-LIMIT
                      BY VALUE WS-ARG-OFFSET
                RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Query failed"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   输出表头
           DISPLAY "ID        DATE        TYPE     CATEGORY"
                   "     AMOUNT       NOTE"
           DISPLAY "--------- ----------  -------  ----------"
                   "  -----------  ----------"

      *>   循环读取结果
           MOVE 0 TO WS-RC
           PERFORM UNTIL WS-RC NOT = 0
               CALL "tx_list_next"
                    USING BY REFERENCE WS-LIST-ID
                          BY REFERENCE WS-LIST-DATE
                          BY REFERENCE WS-LIST-TYPE
                          BY REFERENCE WS-LIST-CATEGORY
                          BY REFERENCE WS-LIST-AMOUNT
                          BY REFERENCE WS-LIST-NOTE
                    RETURNING WS-RC
               IF WS-RC = 0
                   COMPUTE WS-AMOUNT-DISP = WS-LIST-AMOUNT / 100
                   MOVE WS-LIST-ID TO WS-LIST-ID-DISP
                   DISPLAY WS-LIST-ID-DISP "  "
                           FUNCTION TRIM(WS-LIST-DATE) "  "
                           FUNCTION TRIM(WS-LIST-TYPE) "  "
                           FUNCTION TRIM(WS-LIST-CATEGORY) "  "
                           WS-AMOUNT-DISP "  "
                           FUNCTION TRIM(WS-LIST-NOTE)
               END-IF
           END-PERFORM

      *>   结束查询
           CALL "tx_list_end" RETURNING WS-RC.
```

## 6.8 实现更新和删除

```c
int tx_exists(int id) {
    if (db == NULL) return 0;
    
    const char *sql = "SELECT 1 FROM transactions WHERE id = ?;";
    sqlite3_stmt *stmt;
    
    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) != SQLITE_OK) {
        return 0;
    }
    
    sqlite3_bind_int(stmt, 1, id);
    int exists = (sqlite3_step(stmt) == SQLITE_ROW) ? 1 : 0;
    sqlite3_finalize(stmt);
    
    return exists;
}

int tx_delete(int id) {
    if (db == NULL) return RC_DB_ERROR;
    if (!tx_exists(id)) return RC_NOT_FOUND;
    
    const char *sql = "DELETE FROM transactions WHERE id = ?;";
    sqlite3_stmt *stmt;
    
    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) != SQLITE_OK) {
        return RC_DB_ERROR;
    }
    
    sqlite3_bind_int(stmt, 1, id);
    int rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);
    
    return (rc == SQLITE_DONE) ? RC_SUCCESS : RC_DB_ERROR;
}
```

## 6.9 实现报表汇总

```c
int rpt_get_totals(const char *month, long *income, long *expense) {
    if (db == NULL) return RC_DB_ERROR;
    
    char month_buf[16];
    trim_string(month, month_buf, sizeof(month_buf));
    
    *income = 0;
    *expense = 0;
    
    const char *sql = 
        "SELECT tx_type, COALESCE(SUM(amount_cents), 0) "
        "FROM transactions WHERE substr(tx_date, 1, 7) = ? "
        "GROUP BY tx_type;";
    
    sqlite3_stmt *stmt;
    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) != SQLITE_OK) {
        return RC_DB_ERROR;
    }
    
    sqlite3_bind_text(stmt, 1, month_buf, -1, SQLITE_TRANSIENT);
    
    while (sqlite3_step(stmt) == SQLITE_ROW) {
        const char *tx_type = (const char *)sqlite3_column_text(stmt, 0);
        long total = sqlite3_column_int64(stmt, 1);
        
        if (strcmp(tx_type, "INCOME") == 0) {
            *income = total;
        } else if (strcmp(tx_type, "EXPENSE") == 0) {
            *expense = total;
        }
    }
    
    sqlite3_finalize(stmt);
    return RC_SUCCESS;
}
```

## 6.10 练习

1. 实现 `tx_update` 函数，支持部分字段更新

2. 添加一个函数 `tx_get_by_id`，根据 ID 获取单条交易

3. 实现按类别汇总的报表功能

## 下一章预告

下一章我们将把所有功能整合起来，完成完整的 ledger 工具。
