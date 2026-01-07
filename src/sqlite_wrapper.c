/*
 * sqlite_wrapper.c - SQLite3 C 包装函数
 * 供 GnuCOBOL 通过 CALL 调用
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>
#include "sqlite_wrapper.h"

static sqlite3 *db = NULL;
static sqlite3_stmt *list_stmt = NULL;
static sqlite3_stmt *rpt_stmt = NULL;

/* ============================================================
 * 数据库连接管理
 * ============================================================ */

/* 去除字符串尾部空格 */
static char* trim_string(const char *str, char *buf, size_t bufsize) {
    if (!str || !buf) {
        if (buf) buf[0] = '\0';
        return buf;
    }
    strncpy(buf, str, bufsize - 1);
    buf[bufsize - 1] = '\0';
    
    /* 去除尾部空格 */
    size_t len = strlen(buf);
    while (len > 0 && buf[len - 1] == ' ') {
        buf[--len] = '\0';
    }
    return buf;
}

int db_open(const char *db_path) {
    if (db != NULL) {
        return RC_SUCCESS;  /* 已打开 */
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
    if (list_stmt) {
        sqlite3_finalize(list_stmt);
        list_stmt = NULL;
    }
    if (rpt_stmt) {
        sqlite3_finalize(rpt_stmt);
        rpt_stmt = NULL;
    }
    if (db) {
        sqlite3_close(db);
        db = NULL;
    }
    return RC_SUCCESS;
}


/* ============================================================
 * 初始化数据库 Schema
 * ============================================================ */

int db_init_schema(void) {
    if (db == NULL) return RC_DB_ERROR;
    
    char *err_msg = NULL;
    int rc;
    
    /* 创建表 */
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
    
    rc = sqlite3_exec(db, sql_table, NULL, NULL, &err_msg);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", err_msg);
        sqlite3_free(err_msg);
        return RC_DB_ERROR;
    }
    
    /* 创建索引 */
    const char *sql_idx1 = "CREATE INDEX IF NOT EXISTS idx_tx_date ON transactions(tx_date);";
    const char *sql_idx2 = "CREATE INDEX IF NOT EXISTS idx_tx_type ON transactions(tx_type);";
    const char *sql_idx3 = "CREATE INDEX IF NOT EXISTS idx_tx_category ON transactions(category);";
    
    sqlite3_exec(db, sql_idx1, NULL, NULL, NULL);
    sqlite3_exec(db, sql_idx2, NULL, NULL, NULL);
    sqlite3_exec(db, sql_idx3, NULL, NULL, NULL);
    return RC_SUCCESS;
}

/* ============================================================
 * 新增交易
 * ============================================================ */

int tx_add(const char *date, const char *type, const char *category,
           long amount_cents, const char *note) {
    if (db == NULL) return RC_DB_ERROR;
    
    /* trim 输入参数 */
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

/* ============================================================
 * 检查交易是否存在
 * ============================================================ */

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

/* ============================================================
 * 更新交易
 * ============================================================ */

int tx_update(int id, const char *date, const char *type,
              const char *category, long amount_cents, const char *note) {
    if (db == NULL) return RC_DB_ERROR;
    if (!tx_exists(id)) return RC_NOT_FOUND;
    
    /* 动态构建 UPDATE 语句 */
    char sql[1024] = "UPDATE transactions SET updated_at = datetime('now')";
    
    if (date && strlen(date) > 0) {
        strcat(sql, ", tx_date = ?");
    }
    if (type && strlen(type) > 0) {
        strcat(sql, ", tx_type = ?");
    }
    if (category && strlen(category) > 0) {
        strcat(sql, ", category = ?");
    }
    if (amount_cents > 0) {
        strcat(sql, ", amount_cents = ?");
    }
    if (note && strlen(note) > 0) {
        strcat(sql, ", note = ?");
    }
    strcat(sql, " WHERE id = ?;");
    
    sqlite3_stmt *stmt;
    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) != SQLITE_OK) {
        fprintf(stderr, "SQL 准备失败: %s\n", sqlite3_errmsg(db));
        return RC_DB_ERROR;
    }
    
    int idx = 1;
    if (date && strlen(date) > 0) {
        sqlite3_bind_text(stmt, idx++, date, -1, SQLITE_STATIC);
    }
    if (type && strlen(type) > 0) {
        sqlite3_bind_text(stmt, idx++, type, -1, SQLITE_STATIC);
    }
    if (category && strlen(category) > 0) {
        sqlite3_bind_text(stmt, idx++, category, -1, SQLITE_STATIC);
    }
    if (amount_cents > 0) {
        sqlite3_bind_int64(stmt, idx++, amount_cents);
    }
    if (note && strlen(note) > 0) {
        sqlite3_bind_text(stmt, idx++, note, -1, SQLITE_STATIC);
    }
    sqlite3_bind_int(stmt, idx, id);
    
    int rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);
    
    return (rc == SQLITE_DONE) ? RC_SUCCESS : RC_DB_ERROR;
}

/* ============================================================
 * 删除交易
 * ============================================================ */

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


/* ============================================================
 * 查询交易列表 - 开始
 * ============================================================ */

/* 检查 trim 后是否为空 */
static int is_empty(const char *str) {
    if (!str) return 1;
    while (*str) {
        if (*str != ' ') return 0;
        str++;
    }
    return 1;
}

int tx_list_begin(const char *from_date, const char *to_date,
                  const char *month, const char *category,
                  const char *type, const char *keyword,
                  int limit, int offset) {
    if (db == NULL) return RC_DB_ERROR;
    
    if (list_stmt) {
        sqlite3_finalize(list_stmt);
        list_stmt = NULL;
    }
    
    /* trim 输入参数 */
    char from_buf[16], to_buf[16], month_buf[16];
    char cat_buf[64], type_buf[16], kw_buf[64];
    
    trim_string(from_date, from_buf, sizeof(from_buf));
    trim_string(to_date, to_buf, sizeof(to_buf));
    trim_string(month, month_buf, sizeof(month_buf));
    trim_string(category, cat_buf, sizeof(cat_buf));
    trim_string(type, type_buf, sizeof(type_buf));
    trim_string(keyword, kw_buf, sizeof(kw_buf));
    
    /* 构建查询 SQL */
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
    
    if (sqlite3_prepare_v2(db, sql, -1, &list_stmt, NULL) != SQLITE_OK) {
        fprintf(stderr, "SQL prepare failed: %s\n", sqlite3_errmsg(db));
        return RC_DB_ERROR;
    }
    
    /* 绑定参数 - 使用 trim 后的值 */
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

/* ============================================================
 * 查询交易列表 - 获取下一条
 * ============================================================ */

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
        
        /* COBOL 字符串：固定长度，空格填充，无 null terminator */
        memset(date, ' ', 11);
        if (col_date) {
            size_t len = strlen(col_date);
            if (len > 10) len = 10;
            memcpy(date, col_date, len);
        }
        
        memset(type, ' ', 8);
        if (col_type) {
            size_t len = strlen(col_type);
            if (len > 7) len = 7;
            memcpy(type, col_type, len);
        }
        
        memset(category, ' ', 51);
        if (col_cat) {
            size_t len = strlen(col_cat);
            if (len > 50) len = 50;
            memcpy(category, col_cat, len);
        }
        
        *amount_cents = sqlite3_column_int64(list_stmt, 4);
        
        memset(note, ' ', 201);
        if (col_note) {
            size_t len = strlen(col_note);
            if (len > 200) len = 200;
            memcpy(note, col_note, len);
        }
        
        return RC_SUCCESS;
    }
    return RC_NOT_FOUND;
}

/* ============================================================
 * 查询交易列表 - 结束
 * ============================================================ */

int tx_list_end(void) {
    if (list_stmt) {
        sqlite3_finalize(list_stmt);
        list_stmt = NULL;
    }
    return RC_SUCCESS;
}

/* ============================================================
 * 报表 - 获取月度汇总
 * ============================================================ */

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

/* ============================================================
 * 报表 - 按类别汇总开始
 * ============================================================ */

int rpt_category_begin(const char *month) {
    if (db == NULL) return RC_DB_ERROR;
    
    char month_buf[16];
    trim_string(month, month_buf, sizeof(month_buf));
    
    if (rpt_stmt) {
        sqlite3_finalize(rpt_stmt);
        rpt_stmt = NULL;
    }
    
    const char *sql = 
        "SELECT category, tx_type, SUM(amount_cents) "
        "FROM transactions WHERE substr(tx_date, 1, 7) = ? "
        "GROUP BY category, tx_type "
        "ORDER BY tx_type, SUM(amount_cents) DESC;";
    
    if (sqlite3_prepare_v2(db, sql, -1, &rpt_stmt, NULL) != SQLITE_OK) {
        return RC_DB_ERROR;
    }
    
    sqlite3_bind_text(rpt_stmt, 1, month_buf, -1, SQLITE_TRANSIENT);
    return RC_SUCCESS;
}

/* ============================================================
 * 报表 - 获取下一个类别汇总
 * ============================================================ */

int rpt_category_next(char *category, char *type, long *total) {
    if (rpt_stmt == NULL) {
        return RC_NOT_FOUND;
    }
    
    int rc = sqlite3_step(rpt_stmt);
    if (rc == SQLITE_ROW) {
        const char *col_cat = (const char *)sqlite3_column_text(rpt_stmt, 0);
        const char *col_type = (const char *)sqlite3_column_text(rpt_stmt, 1);
        
        /* 复制 category - COBOL PIC X(51)，用空格填充 */
        memset(category, ' ', 51);
        if (col_cat) {
            size_t len = strlen(col_cat);
            if (len > 50) len = 50;
            memcpy(category, col_cat, len);
        }
        
        /* 复制 type - COBOL PIC X(8)，用空格填充 */
        memset(type, ' ', 8);
        if (col_type) {
            size_t len = strlen(col_type);
            if (len > 8) len = 8;
            memcpy(type, col_type, len);
        }
        
        *total = sqlite3_column_int64(rpt_stmt, 2);
        return RC_SUCCESS;
    }
    return RC_NOT_FOUND;
}

/* ============================================================
 * 报表 - 类别汇总结束
 * ============================================================ */

int rpt_category_end(void) {
    if (rpt_stmt) {
        sqlite3_finalize(rpt_stmt);
        rpt_stmt = NULL;
    }
    return RC_SUCCESS;
}


/* ============================================================
 * CSV 导出辅助函数
 * ============================================================ */

static void write_csv_field(FILE *fp, const char *field) {
    int needs_quote = 0;
    const char *p;
    
    /* 检查是否需要引号 */
    for (p = field; *p; p++) {
        if (*p == ',' || *p == '"' || *p == '\n' || *p == '\r') {
            needs_quote = 1;
            break;
        }
    }
    
    if (needs_quote) {
        fputc('"', fp);
        for (p = field; *p; p++) {
            if (*p == '"') {
                fputs("\"\"", fp);
            } else {
                fputc(*p, fp);
            }
        }
        fputc('"', fp);
    } else {
        fputs(field, fp);
    }
}

/* ============================================================
 * 导出交易明细 CSV
 * ============================================================ */

int export_tx_csv(const char *filepath,
                  const char *from_date, const char *to_date,
                  const char *month, const char *category,
                  const char *type, const char *keyword,
                  int limit, int offset) {
    /* trim filepath */
    char path_buf[256];
    trim_string(filepath, path_buf, sizeof(path_buf));
    
    FILE *fp = fopen(path_buf, "w");
    if (!fp) {
        fprintf(stderr, "Cannot create file: %s\n", path_buf);
        return RC_IO_ERROR;
    }
    
    /* 写入表头 */
    fprintf(fp, "ID,Date,Type,Category,Amount,Note\n");
    
    /* trim 输入参数 */
    char from_buf[16], to_buf[16], month_buf[16];
    char cat_buf[64], type_buf[16], kw_buf[64];
    
    trim_string(from_date, from_buf, sizeof(from_buf));
    trim_string(to_date, to_buf, sizeof(to_buf));
    trim_string(month, month_buf, sizeof(month_buf));
    trim_string(category, cat_buf, sizeof(cat_buf));
    trim_string(type, type_buf, sizeof(type_buf));
    trim_string(keyword, kw_buf, sizeof(kw_buf));
    
    /* 构建查询 SQL */
    char sql[2048];
    strcpy(sql, "SELECT id, tx_date, tx_type, category, amount_cents, "
                "COALESCE(note, '') FROM transactions WHERE 1=1");
    
    if (strlen(from_buf) > 0) strcat(sql, " AND tx_date >= ?");
    if (strlen(to_buf) > 0) strcat(sql, " AND tx_date <= ?");
    if (strlen(month_buf) > 0) strcat(sql, " AND substr(tx_date, 1, 7) = ?");
    if (strlen(cat_buf) > 0) strcat(sql, " AND category = ?");
    if (strlen(type_buf) > 0) strcat(sql, " AND tx_type = ?");
    if (strlen(kw_buf) > 0) strcat(sql, " AND (note LIKE ? OR category LIKE ?)");
    strcat(sql, " ORDER BY tx_date DESC, id DESC LIMIT ? OFFSET ?;");
    
    sqlite3_stmt *stmt;
    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) != SQLITE_OK) {
        fclose(fp);
        return RC_DB_ERROR;
    }
    
    int idx = 1;
    if (strlen(from_buf) > 0) sqlite3_bind_text(stmt, idx++, from_buf, -1, SQLITE_TRANSIENT);
    if (strlen(to_buf) > 0) sqlite3_bind_text(stmt, idx++, to_buf, -1, SQLITE_TRANSIENT);
    if (strlen(month_buf) > 0) sqlite3_bind_text(stmt, idx++, month_buf, -1, SQLITE_TRANSIENT);
    if (strlen(cat_buf) > 0) sqlite3_bind_text(stmt, idx++, cat_buf, -1, SQLITE_TRANSIENT);
    if (strlen(type_buf) > 0) sqlite3_bind_text(stmt, idx++, type_buf, -1, SQLITE_TRANSIENT);
    if (strlen(kw_buf) > 0) {
        char like_pattern[256];
        snprintf(like_pattern, sizeof(like_pattern), "%%%s%%", kw_buf);
        sqlite3_bind_text(stmt, idx++, like_pattern, -1, SQLITE_TRANSIENT);
        sqlite3_bind_text(stmt, idx++, like_pattern, -1, SQLITE_TRANSIENT);
    }
    sqlite3_bind_int(stmt, idx++, limit > 0 ? limit : 50);
    sqlite3_bind_int(stmt, idx, offset >= 0 ? offset : 0);
    
    while (sqlite3_step(stmt) == SQLITE_ROW) {
        int id = sqlite3_column_int(stmt, 0);
        const char *date = (const char *)sqlite3_column_text(stmt, 1);
        const char *tx_type = (const char *)sqlite3_column_text(stmt, 2);
        const char *cat = (const char *)sqlite3_column_text(stmt, 3);
        long amount = sqlite3_column_int64(stmt, 4);
        const char *note = (const char *)sqlite3_column_text(stmt, 5);
        
        fprintf(fp, "%d,", id);
        write_csv_field(fp, date ? date : "");
        fputc(',', fp);
        write_csv_field(fp, tx_type ? tx_type : "");
        fputc(',', fp);
        write_csv_field(fp, cat ? cat : "");
        fprintf(fp, ",%.2f,", amount / 100.0);
        write_csv_field(fp, note ? note : "");
        fputc('\n', fp);
    }
    
    sqlite3_finalize(stmt);
    fclose(fp);
    return RC_SUCCESS;
}

/* ============================================================
 * 导出月度报表 CSV
 * ============================================================ */

int export_report_csv(const char *filepath, const char *month) {
    /* trim 输入参数 */
    char path_buf[256], month_buf[16];
    trim_string(filepath, path_buf, sizeof(path_buf));
    trim_string(month, month_buf, sizeof(month_buf));
    
    FILE *fp = fopen(path_buf, "w");
    if (!fp) {
        fprintf(stderr, "Cannot create file: %s\n", path_buf);
        return RC_IO_ERROR;
    }
    
    long income = 0, expense = 0;
    rpt_get_totals(month_buf, &income, &expense);
    long net = income - expense;
    
    /* 写入汇总 */
    fprintf(fp, "Month,%s\n", month_buf);
    fprintf(fp, "Total Income,%.2f\n", income / 100.0);
    fprintf(fp, "Total Expense,%.2f\n", expense / 100.0);
    fprintf(fp, "Net,%.2f\n", net / 100.0);
    fprintf(fp, "\n");
    
    /* 写入类别汇总 - 直接从数据库读取 */
    fprintf(fp, "Category,Type,Amount\n");
    
    const char *sql = 
        "SELECT category, tx_type, SUM(amount_cents) "
        "FROM transactions WHERE substr(tx_date, 1, 7) = ? "
        "GROUP BY category, tx_type "
        "ORDER BY tx_type, SUM(amount_cents) DESC;";
    
    sqlite3_stmt *stmt;
    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        sqlite3_bind_text(stmt, 1, month_buf, -1, SQLITE_TRANSIENT);
        
        while (sqlite3_step(stmt) == SQLITE_ROW) {
            const char *cat = (const char *)sqlite3_column_text(stmt, 0);
            const char *type = (const char *)sqlite3_column_text(stmt, 1);
            long total = sqlite3_column_int64(stmt, 2);
            
            write_csv_field(fp, cat ? cat : "");
            fputc(',', fp);
            write_csv_field(fp, type ? type : "");
            fprintf(fp, ",%.2f\n", total / 100.0);
        }
        sqlite3_finalize(stmt);
    }
    
    fclose(fp);
    return RC_SUCCESS;
}
