      *> ============================================================
      *> common.cpy - 常量、通用结构、错误码定义
      *> ============================================================

      *> 返回码常量
       01 WS-RC-SUCCESS            PIC 9 VALUE 0.
       01 WS-RC-PARAM-ERROR        PIC 9 VALUE 1.
       01 WS-RC-DB-ERROR           PIC 9 VALUE 2.
       01 WS-RC-NOT-FOUND          PIC 9 VALUE 3.
       01 WS-RC-IO-ERROR           PIC 9 VALUE 4.

      *> 交易类型常量
       01 WS-TYPE-INCOME           PIC X(7) VALUE "INCOME".
       01 WS-TYPE-EXPENSE          PIC X(7) VALUE "EXPENSE".

      *> 路径常量
       01 WS-DEFAULT-DB-PATH       PIC X(50) VALUE "./db/ledger.db".
       01 WS-DEFAULT-EXPORT-DIR    PIC X(20) VALUE "./export/".

      *> 分页默认值
       01 WS-DEFAULT-LIMIT         PIC 9(4) VALUE 50.
       01 WS-DEFAULT-OFFSET        PIC 9(8) VALUE 0.

      *> 字段长度限制
       01 WS-MAX-CATEGORY-LEN      PIC 9(2) VALUE 50.
       01 WS-MAX-NOTE-LEN          PIC 9(3) VALUE 200.

      *> 通用工作变量
       01 WS-RETURN-CODE           PIC 9 VALUE 0.
       01 WS-ERROR-MSG             PIC X(200) VALUE SPACES.
       01 WS-TEMP-STRING           PIC X(500) VALUE SPACES.
