      *> ============================================================
      *> types.cpy - 数据结构定义
      *> ============================================================

      *> 交易记录结构
       01 WS-TRANSACTION.
          05 WS-TX-ID              PIC 9(10) VALUE 0.
          05 WS-TX-DATE            PIC X(10) VALUE SPACES.
          05 WS-TX-TYPE            PIC X(7)  VALUE SPACES.
          05 WS-TX-CATEGORY        PIC X(50) VALUE SPACES.
          05 WS-TX-AMOUNT-CENTS    PIC S9(12) VALUE 0.
          05 WS-TX-NOTE            PIC X(200) VALUE SPACES.
          05 WS-TX-CREATED-AT      PIC X(19) VALUE SPACES.
          05 WS-TX-UPDATED-AT      PIC X(19) VALUE SPACES.

      *> 金额显示用（元，两位小数）
       01 WS-AMOUNT-DISPLAY        PIC Z(9)9.99.
       01 WS-AMOUNT-NUMERIC        PIC 9(10)V99 VALUE 0.

      *> 查询过滤条件
       01 WS-FILTER.
          05 WS-FILTER-FROM        PIC X(10) VALUE SPACES.
          05 WS-FILTER-TO          PIC X(10) VALUE SPACES.
          05 WS-FILTER-MONTH       PIC X(7)  VALUE SPACES.
          05 WS-FILTER-CATEGORY    PIC X(50) VALUE SPACES.
          05 WS-FILTER-TYPE        PIC X(7)  VALUE SPACES.
          05 WS-FILTER-KEYWORD     PIC X(50) VALUE SPACES.
          05 WS-FILTER-LIMIT       PIC 9(4)  VALUE 50.
          05 WS-FILTER-OFFSET      PIC 9(8)  VALUE 0.

      *> 报表汇总结构
       01 WS-REPORT-SUMMARY.
          05 WS-RPT-MONTH          PIC X(7)  VALUE SPACES.
          05 WS-RPT-TOTAL-INCOME   PIC S9(12) VALUE 0.
          05 WS-RPT-TOTAL-EXPENSE  PIC S9(12) VALUE 0.
          05 WS-RPT-NET            PIC S9(12) VALUE 0.

      *> 类别汇总结构（用于报表）
       01 WS-CATEGORY-SUMMARY.
          05 WS-CAT-NAME           PIC X(50) VALUE SPACES.
          05 WS-CAT-TYPE           PIC X(7)  VALUE SPACES.
          05 WS-CAT-TOTAL          PIC S9(12) VALUE 0.

      *> CLI 命令枚举
       01 WS-COMMAND               PIC X(20) VALUE SPACES.
       01 WS-CMD-INIT              PIC X(20) VALUE "init".
       01 WS-CMD-ADD               PIC X(20) VALUE "add".
       01 WS-CMD-LIST              PIC X(20) VALUE "list".
       01 WS-CMD-UPDATE            PIC X(20) VALUE "update".
       01 WS-CMD-DELETE            PIC X(20) VALUE "delete".
       01 WS-CMD-REPORT            PIC X(20) VALUE "report".
       01 WS-CMD-EXPORT            PIC X(20) VALUE "export".
       01 WS-CMD-HELP              PIC X(20) VALUE "help".

      *> 数据库路径
       01 WS-DB-PATH               PIC X(200) VALUE SPACES.
       01 WS-EXPORT-PATH           PIC X(200) VALUE SPACES.
