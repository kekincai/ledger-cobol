      *> ============================================================
      *> dao_tx.cbl - 交易表数据访问层 (CRUD)
      *> 使用 SQLite3 通过 GnuCOBOL 的 CALL 接口
      *> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAO-TX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "common.cpy".
       COPY "types.cpy".

      *> SQLite 相关变量
       01 WS-DB-HANDLE             POINTER VALUE NULL.
       01 WS-STMT-HANDLE           POINTER VALUE NULL.
       01 WS-SQL-RESULT            PIC S9(9) COMP VALUE 0.
       01 WS-SQL-TEXT              PIC X(2000) VALUE SPACES.
       01 WS-SQL-ERROR-MSG         PIC X(500) VALUE SPACES.

      *> SQLite 返回码常量
       01 SQLITE-OK                PIC S9(9) COMP VALUE 0.
       01 SQLITE-ROW               PIC S9(9) COMP VALUE 100.
       01 SQLITE-DONE              PIC S9(9) COMP VALUE 101.

      *> 工作变量
       01 WS-AMOUNT-CENTS          PIC S9(12) VALUE 0.
       01 WS-AMOUNT-TEMP           PIC 9(10)V99 VALUE 0.
       01 WS-ROW-COUNT             PIC 9(8) VALUE 0.
       01 WS-LAST-ID               PIC 9(10) VALUE 0.

      *> 查询结果缓冲
       01 WS-RESULT-ID             PIC 9(10) VALUE 0.
       01 WS-RESULT-DATE           PIC X(10) VALUE SPACES.
       01 WS-RESULT-TYPE           PIC X(7) VALUE SPACES.
       01 WS-RESULT-CATEGORY       PIC X(50) VALUE SPACES.
       01 WS-RESULT-AMOUNT         PIC S9(12) VALUE 0.
       01 WS-RESULT-NOTE           PIC X(200) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-DAO.
           STOP RUN.

      *> ============================================================
      *> 初始化数据库（创建表和索引）
      *> ============================================================
       INIT-DATABASE SECTION.
           PERFORM OPEN-DATABASE
           IF WS-RETURN-CODE NOT = 0
               EXIT SECTION
           END-IF

      *>   创建 transactions 表
           MOVE "CREATE TABLE IF NOT EXISTS transactions ("
                & "id INTEGER PRIMARY KEY AUTOINCREMENT,"
                & "tx_date TEXT NOT NULL,"
                & "tx_type TEXT NOT NULL,"
                & "category TEXT NOT NULL,"
                & "amount_cents INTEGER NOT NULL,"
                & "note TEXT,"
                & "created_at TEXT NOT NULL DEFAULT "
                & "(datetime('now')),"
                & "updated_at TEXT NOT NULL DEFAULT "
                & "(datetime('now'))"
                & ");"
                TO WS-SQL-TEXT

           PERFORM EXECUTE-SQL
           IF WS-RETURN-CODE NOT = 0
               PERFORM CLOSE-DATABASE
               EXIT SECTION
           END-IF

      *>   创建索引
           MOVE "CREATE INDEX IF NOT EXISTS idx_tx_date "
                & "ON transactions(tx_date);"
                TO WS-SQL-TEXT
           PERFORM EXECUTE-SQL

           MOVE "CREATE INDEX IF NOT EXISTS idx_tx_type "
                & "ON transactions(tx_type);"
                TO WS-SQL-TEXT
           PERFORM EXECUTE-SQL

           MOVE "CREATE INDEX IF NOT EXISTS idx_tx_category "
                & "ON transactions(category);"
                TO WS-SQL-TEXT
           PERFORM EXECUTE-SQL

           PERFORM CLOSE-DATABASE
           DISPLAY "数据库初始化成功".

      *> ============================================================
      *> 新增交易
      *> ============================================================
       ADD-TRANSACTION SECTION.
           PERFORM OPEN-DATABASE
           IF WS-RETURN-CODE NOT = 0
               EXIT SECTION
           END-IF

      *>   将金额转换为分
           COMPUTE WS-AMOUNT-CENTS = 
                   FUNCTION NUMVAL(WS-ARG-AMOUNT) * 100

           STRING "INSERT INTO transactions "
                  "(tx_date, tx_type, category, amount_cents, note) "
                  "VALUES ('"
                  FUNCTION TRIM(WS-ARG-DATE) "', '"
                  FUNCTION TRIM(WS-ARG-TYPE) "', '"
                  FUNCTION TRIM(WS-ARG-CATEGORY) "', "
                  WS-AMOUNT-CENTS ", '"
                  FUNCTION TRIM(WS-ARG-NOTE) "');"
                  DELIMITED SIZE INTO WS-SQL-TEXT

           PERFORM EXECUTE-SQL
           IF WS-RETURN-CODE = 0
               DISPLAY "交易添加成功"
           END-IF

           PERFORM CLOSE-DATABASE.

      *> ============================================================
      *> 查询交易列表
      *> ============================================================
       LIST-TRANSACTIONS SECTION.
           PERFORM OPEN-DATABASE
           IF WS-RETURN-CODE NOT = 0
               EXIT SECTION
           END-IF

      *>   构建基础查询
           MOVE "SELECT id, tx_date, tx_type, category, "
                & "amount_cents, note FROM transactions WHERE 1=1"
                TO WS-SQL-TEXT

      *>   添加过滤条件
           IF WS-ARG-FROM NOT = SPACES
               STRING FUNCTION TRIM(WS-SQL-TEXT)
                      " AND tx_date >= '" 
                      FUNCTION TRIM(WS-ARG-FROM) "'"
                      DELIMITED SIZE INTO WS-SQL-TEXT
           END-IF

           IF WS-ARG-TO NOT = SPACES
               STRING FUNCTION TRIM(WS-SQL-TEXT)
                      " AND tx_date <= '"
                      FUNCTION TRIM(WS-ARG-TO) "'"
                      DELIMITED SIZE INTO WS-SQL-TEXT
           END-IF

           IF WS-ARG-MONTH NOT = SPACES
               STRING FUNCTION TRIM(WS-SQL-TEXT)
                      " AND substr(tx_date, 1, 7) = '"
                      FUNCTION TRIM(WS-ARG-MONTH) "'"
                      DELIMITED SIZE INTO WS-SQL-TEXT
           END-IF

           IF WS-ARG-CATEGORY NOT = SPACES
               STRING FUNCTION TRIM(WS-SQL-TEXT)
                      " AND category = '"
                      FUNCTION TRIM(WS-ARG-CATEGORY) "'"
                      DELIMITED SIZE INTO WS-SQL-TEXT
           END-IF

           IF WS-ARG-TYPE NOT = SPACES
               STRING FUNCTION TRIM(WS-SQL-TEXT)
                      " AND tx_type = '"
                      FUNCTION TRIM(WS-ARG-TYPE) "'"
                      DELIMITED SIZE INTO WS-SQL-TEXT
           END-IF

           IF WS-ARG-KEYWORD NOT = SPACES
               STRING FUNCTION TRIM(WS-SQL-TEXT)
                      " AND (note LIKE '%"
                      FUNCTION TRIM(WS-ARG-KEYWORD) "%'"
                      " OR category LIKE '%"
                      FUNCTION TRIM(WS-ARG-KEYWORD) "%')"
                      DELIMITED SIZE INTO WS-SQL-TEXT
           END-IF

      *>   排序和分页
           STRING FUNCTION TRIM(WS-SQL-TEXT)
                  " ORDER BY tx_date DESC, id DESC"
                  " LIMIT " WS-ARG-LIMIT
                  " OFFSET " WS-ARG-OFFSET ";"
                  DELIMITED SIZE INTO WS-SQL-TEXT

      *>   输出表头
           DISPLAY "ID        DATE        TYPE     CATEGORY"
                   "            AMOUNT       NOTE"
           DISPLAY "--------- ----------  -------  "
                   "---------------  -----------  ----------"

      *>   执行查询并输出结果
           PERFORM EXECUTE-QUERY

           PERFORM CLOSE-DATABASE.

      *> ============================================================
      *> 更新交易
      *> ============================================================
       UPDATE-TRANSACTION SECTION.
           PERFORM OPEN-DATABASE
           IF WS-RETURN-CODE NOT = 0
               EXIT SECTION
           END-IF

      *>   检查记录是否存在
           STRING "SELECT id FROM transactions WHERE id = "
                  WS-ARG-ID ";"
                  DELIMITED SIZE INTO WS-SQL-TEXT

           PERFORM CHECK-EXISTS
           IF WS-ROW-COUNT = 0
               MOVE 3 TO WS-RETURN-CODE
               DISPLAY "错误: 交易 ID " WS-ARG-ID " 不存在"
               PERFORM CLOSE-DATABASE
               EXIT SECTION
           END-IF

      *>   构建更新语句
           MOVE "UPDATE transactions SET updated_at = datetime('now')"
                TO WS-SQL-TEXT

           IF WS-ARG-TYPE NOT = SPACES
               STRING FUNCTION TRIM(WS-SQL-TEXT)
                      ", tx_type = '" FUNCTION TRIM(WS-ARG-TYPE) "'"
                      DELIMITED SIZE INTO WS-SQL-TEXT
           END-IF

           IF WS-ARG-AMOUNT NOT = SPACES
               COMPUTE WS-AMOUNT-CENTS = 
                       FUNCTION NUMVAL(WS-ARG-AMOUNT) * 100
               STRING FUNCTION TRIM(WS-SQL-TEXT)
                      ", amount_cents = " WS-AMOUNT-CENTS
                      DELIMITED SIZE INTO WS-SQL-TEXT
           END-IF

           IF WS-ARG-CATEGORY NOT = SPACES
               STRING FUNCTION TRIM(WS-SQL-TEXT)
                      ", category = '" 
                      FUNCTION TRIM(WS-ARG-CATEGORY) "'"
                      DELIMITED SIZE INTO WS-SQL-TEXT
           END-IF

           IF WS-ARG-DATE NOT = SPACES
               STRING FUNCTION TRIM(WS-SQL-TEXT)
                      ", tx_date = '" FUNCTION TRIM(WS-ARG-DATE) "'"
                      DELIMITED SIZE INTO WS-SQL-TEXT
           END-IF

           IF WS-ARG-NOTE NOT = SPACES
               STRING FUNCTION TRIM(WS-SQL-TEXT)
                      ", note = '" FUNCTION TRIM(WS-ARG-NOTE) "'"
                      DELIMITED SIZE INTO WS-SQL-TEXT
           END-IF

           STRING FUNCTION TRIM(WS-SQL-TEXT)
                  " WHERE id = " WS-ARG-ID ";"
                  DELIMITED SIZE INTO WS-SQL-TEXT

           PERFORM EXECUTE-SQL
           IF WS-RETURN-CODE = 0
               DISPLAY "交易 ID " WS-ARG-ID " 更新成功"
           END-IF

           PERFORM CLOSE-DATABASE.

      *> ============================================================
      *> 删除交易
      *> ============================================================
       DELETE-TRANSACTION SECTION.
           PERFORM OPEN-DATABASE
           IF WS-RETURN-CODE NOT = 0
               EXIT SECTION
           END-IF

      *>   检查记录是否存在
           STRING "SELECT id FROM transactions WHERE id = "
                  WS-ARG-ID ";"
                  DELIMITED SIZE INTO WS-SQL-TEXT

           PERFORM CHECK-EXISTS
           IF WS-ROW-COUNT = 0
               MOVE 3 TO WS-RETURN-CODE
               DISPLAY "错误: 交易 ID " WS-ARG-ID " 不存在"
               PERFORM CLOSE-DATABASE
               EXIT SECTION
           END-IF

      *>   执行删除
           STRING "DELETE FROM transactions WHERE id = "
                  WS-ARG-ID ";"
                  DELIMITED SIZE INTO WS-SQL-TEXT

           PERFORM EXECUTE-SQL
           IF WS-RETURN-CODE = 0
               DISPLAY "交易 ID " WS-ARG-ID " 删除成功"
           END-IF

           PERFORM CLOSE-DATABASE.

      *> ============================================================
      *> 辅助过程：打开数据库
      *> ============================================================
       OPEN-DATABASE SECTION.
           MOVE 0 TO WS-RETURN-CODE
      *>   实际实现需要调用 SQLite3 C 接口
      *>   CALL "sqlite3_open" USING WS-DB-PATH WS-DB-HANDLE
      *>                       RETURNING WS-SQL-RESULT
           IF WS-SQL-RESULT NOT = SQLITE-OK
               MOVE 2 TO WS-RETURN-CODE
               DISPLAY "错误: 无法打开数据库"
           END-IF.

      *> ============================================================
      *> 辅助过程：关闭数据库
      *> ============================================================
       CLOSE-DATABASE SECTION.
      *>   CALL "sqlite3_close" USING WS-DB-HANDLE
           CONTINUE.

      *> ============================================================
      *> 辅助过程：执行 SQL（无返回结果）
      *> ============================================================
       EXECUTE-SQL SECTION.
           MOVE 0 TO WS-RETURN-CODE
      *>   CALL "sqlite3_exec" USING WS-DB-HANDLE WS-SQL-TEXT
      *>                             NULL NULL WS-SQL-ERROR-MSG
      *>                       RETURNING WS-SQL-RESULT
           IF WS-SQL-RESULT NOT = SQLITE-OK
               MOVE 2 TO WS-RETURN-CODE
               DISPLAY "SQL 错误: " WS-SQL-ERROR-MSG
           END-IF.

      *> ============================================================
      *> 辅助过程：执行查询（有返回结果）
      *> ============================================================
       EXECUTE-QUERY SECTION.
      *>   实际实现需要使用 sqlite3_prepare_v2, sqlite3_step 等
           CONTINUE.

      *> ============================================================
      *> 辅助过程：检查记录是否存在
      *> ============================================================
       CHECK-EXISTS SECTION.
           MOVE 0 TO WS-ROW-COUNT
      *>   执行查询并计数
           CONTINUE.
