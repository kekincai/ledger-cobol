/*
 * sqlite_wrapper.h - SQLite3 C 包装函数头文件
 * 供 GnuCOBOL 通过 CALL 调用
 */

#ifndef SQLITE_WRAPPER_H
#define SQLITE_WRAPPER_H

/* 返回码 */
#define RC_SUCCESS      0
#define RC_PARAM_ERROR  1
#define RC_DB_ERROR     2
#define RC_NOT_FOUND    3
#define RC_IO_ERROR     4

/* 数据库操作 */
int db_open(const char *db_path);
int db_close(void);
int db_init_schema(void);

/* 交易 CRUD */
int tx_add(const char *date, const char *type, const char *category,
           long amount_cents, const char *note);
int tx_update(int id, const char *date, const char *type, 
              const char *category, long amount_cents, const char *note);
int tx_delete(int id);
int tx_exists(int id);

/* 查询 */
int tx_list_begin(const char *from_date, const char *to_date,
                  const char *month, const char *category,
                  const char *type, const char *keyword,
                  int limit, int offset);
int tx_list_next(int *id, char *date, char *type, char *category,
                 long *amount_cents, char *note);
int tx_list_end(void);

/* 报表 */
int rpt_get_totals(const char *month, long *income, long *expense);
int rpt_category_begin(const char *month);
int rpt_category_next(char *category, char *type, long *total);
int rpt_category_end(void);

/* CSV 导出 */
int export_tx_csv(const char *filepath,
                  const char *from_date, const char *to_date,
                  const char *month, const char *category,
                  const char *type, const char *keyword,
                  int limit, int offset);
int export_report_csv(const char *filepath, const char *month);

#endif
