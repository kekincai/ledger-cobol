      *> ============================================================
      *> validate.cbl - 输入校验模块
      *> 校验日期、金额、类型、类别等输入
      *> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "common.cpy".

      *> 日期校验工作变量
       01 WS-DATE-WORK.
          05 WS-DATE-YEAR          PIC 9(4).
          05 WS-DATE-SEP1          PIC X.
          05 WS-DATE-MONTH         PIC 9(2).
          05 WS-DATE-SEP2          PIC X.
          05 WS-DATE-DAY           PIC 9(2).

       01 WS-YEAR-NUM              PIC 9(4).
       01 WS-MONTH-NUM             PIC 9(2).
       01 WS-DAY-NUM               PIC 9(2).
       01 WS-MAX-DAY               PIC 9(2).
       01 WS-IS-LEAP               PIC 9 VALUE 0.
       01 WS-REMAINDER             PIC 9(4).

      *> 金额校验工作变量
       01 WS-AMOUNT-STR            PIC X(20).
       01 WS-AMOUNT-LEN            PIC 9(2).
       01 WS-CHAR-IDX              PIC 9(2).
       01 WS-CURRENT-CHAR          PIC X.
       01 WS-DOT-COUNT             PIC 9 VALUE 0.
       01 WS-DECIMAL-PLACES        PIC 9 VALUE 0.
       01 WS-AFTER-DOT             PIC 9 VALUE 0.

      *> 通用校验结果
       01 WS-VALID-FLAG            PIC 9 VALUE 1.
          88 IS-VALID              VALUE 1.
          88 IS-INVALID            VALUE 0.

       LINKAGE SECTION.
      *> 输入参数
       01 LS-INPUT-VALUE           PIC X(200).
       01 LS-VALIDATE-TYPE         PIC X(10).
      *> 输出参数
       01 LS-RESULT                PIC 9.
       01 LS-ERROR-MSG             PIC X(100).

       PROCEDURE DIVISION USING LS-INPUT-VALUE LS-VALIDATE-TYPE
                                LS-RESULT LS-ERROR-MSG.
       MAIN-VALIDATE.
           MOVE 1 TO LS-RESULT
           MOVE SPACES TO LS-ERROR-MSG

           EVALUATE TRUE
               WHEN LS-VALIDATE-TYPE = "DATE"
                   PERFORM VALIDATE-DATE
               WHEN LS-VALIDATE-TYPE = "MONTH"
                   PERFORM VALIDATE-MONTH
               WHEN LS-VALIDATE-TYPE = "AMOUNT"
                   PERFORM VALIDATE-AMOUNT
               WHEN LS-VALIDATE-TYPE = "TYPE"
                   PERFORM VALIDATE-TYPE
               WHEN LS-VALIDATE-TYPE = "CATEGORY"
                   PERFORM VALIDATE-CATEGORY
               WHEN OTHER
                   MOVE 0 TO LS-RESULT
                   MOVE "未知的校验类型" TO LS-ERROR-MSG
           END-EVALUATE

           STOP RUN.

      *> ============================================================
      *> 校验日期格式 YYYY-MM-DD
      *> ============================================================
       VALIDATE-DATE.
           IF FUNCTION LENGTH(FUNCTION TRIM(LS-INPUT-VALUE)) 
              NOT = 10
               MOVE 0 TO LS-RESULT
               MOVE "日期格式错误，应为 YYYY-MM-DD" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           MOVE LS-INPUT-VALUE(1:10) TO WS-DATE-WORK

           IF WS-DATE-SEP1 NOT = "-" OR WS-DATE-SEP2 NOT = "-"
               MOVE 0 TO LS-RESULT
               MOVE "日期分隔符错误，应使用 -" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           IF WS-DATE-YEAR NOT NUMERIC
               MOVE 0 TO LS-RESULT
               MOVE "年份必须是数字" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           IF WS-DATE-MONTH NOT NUMERIC
               MOVE 0 TO LS-RESULT
               MOVE "月份必须是数字" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           IF WS-DATE-DAY NOT NUMERIC
               MOVE 0 TO LS-RESULT
               MOVE "日期必须是数字" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           MOVE WS-DATE-YEAR TO WS-YEAR-NUM
           MOVE WS-DATE-MONTH TO WS-MONTH-NUM
           MOVE WS-DATE-DAY TO WS-DAY-NUM

           IF WS-YEAR-NUM < 1900 OR WS-YEAR-NUM > 2100
               MOVE 0 TO LS-RESULT
               MOVE "年份超出有效范围 (1900-2100)" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           IF WS-MONTH-NUM < 1 OR WS-MONTH-NUM > 12
               MOVE 0 TO LS-RESULT
               MOVE "月份必须在 1-12 之间" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

      *>   计算该月最大天数
           PERFORM CALC-MAX-DAY

           IF WS-DAY-NUM < 1 OR WS-DAY-NUM > WS-MAX-DAY
               MOVE 0 TO LS-RESULT
               STRING "日期必须在 1-" DELIMITED SIZE
                      WS-MAX-DAY DELIMITED SIZE
                      " 之间" DELIMITED SIZE
                      INTO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO LS-RESULT.

      *> ============================================================
      *> 计算某月最大天数
      *> ============================================================
       CALC-MAX-DAY.
           MOVE 0 TO WS-IS-LEAP

      *>   判断闰年
           DIVIDE WS-YEAR-NUM BY 400 GIVING WS-REMAINDER 
                  REMAINDER WS-REMAINDER
           IF WS-REMAINDER = 0
               MOVE 1 TO WS-IS-LEAP
           ELSE
               DIVIDE WS-YEAR-NUM BY 100 GIVING WS-REMAINDER
                      REMAINDER WS-REMAINDER
               IF WS-REMAINDER = 0
                   MOVE 0 TO WS-IS-LEAP
               ELSE
                   DIVIDE WS-YEAR-NUM BY 4 GIVING WS-REMAINDER
                          REMAINDER WS-REMAINDER
                   IF WS-REMAINDER = 0
                       MOVE 1 TO WS-IS-LEAP
                   END-IF
               END-IF
           END-IF

           EVALUATE WS-MONTH-NUM
               WHEN 1  MOVE 31 TO WS-MAX-DAY
               WHEN 2  
                   IF WS-IS-LEAP = 1
                       MOVE 29 TO WS-MAX-DAY
                   ELSE
                       MOVE 28 TO WS-MAX-DAY
                   END-IF
               WHEN 3  MOVE 31 TO WS-MAX-DAY
               WHEN 4  MOVE 30 TO WS-MAX-DAY
               WHEN 5  MOVE 31 TO WS-MAX-DAY
               WHEN 6  MOVE 30 TO WS-MAX-DAY
               WHEN 7  MOVE 31 TO WS-MAX-DAY
               WHEN 8  MOVE 31 TO WS-MAX-DAY
               WHEN 9  MOVE 30 TO WS-MAX-DAY
               WHEN 10 MOVE 31 TO WS-MAX-DAY
               WHEN 11 MOVE 30 TO WS-MAX-DAY
               WHEN 12 MOVE 31 TO WS-MAX-DAY
           END-EVALUATE.

      *> ============================================================
      *> 校验月份格式 YYYY-MM
      *> ============================================================
       VALIDATE-MONTH.
           IF FUNCTION LENGTH(FUNCTION TRIM(LS-INPUT-VALUE)) NOT = 7
               MOVE 0 TO LS-RESULT
               MOVE "月份格式错误，应为 YYYY-MM" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           IF LS-INPUT-VALUE(5:1) NOT = "-"
               MOVE 0 TO LS-RESULT
               MOVE "月份分隔符错误，应使用 -" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           IF LS-INPUT-VALUE(1:4) NOT NUMERIC
               MOVE 0 TO LS-RESULT
               MOVE "年份必须是数字" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           IF LS-INPUT-VALUE(6:2) NOT NUMERIC
               MOVE 0 TO LS-RESULT
               MOVE "月份必须是数字" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           MOVE LS-INPUT-VALUE(1:4) TO WS-YEAR-NUM
           MOVE LS-INPUT-VALUE(6:2) TO WS-MONTH-NUM

           IF WS-YEAR-NUM < 1900 OR WS-YEAR-NUM > 2100
               MOVE 0 TO LS-RESULT
               MOVE "年份超出有效范围 (1900-2100)" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           IF WS-MONTH-NUM < 1 OR WS-MONTH-NUM > 12
               MOVE 0 TO LS-RESULT
               MOVE "月份必须在 1-12 之间" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO LS-RESULT.

      *> ============================================================
      *> 校验金额（正数，最多两位小数）
      *> ============================================================
       VALIDATE-AMOUNT.
           MOVE FUNCTION TRIM(LS-INPUT-VALUE) TO WS-AMOUNT-STR
           MOVE FUNCTION LENGTH(FUNCTION TRIM(LS-INPUT-VALUE)) 
                TO WS-AMOUNT-LEN

           IF WS-AMOUNT-LEN = 0
               MOVE 0 TO LS-RESULT
               MOVE "金额不能为空" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-DOT-COUNT
           MOVE 0 TO WS-DECIMAL-PLACES
           MOVE 0 TO WS-AFTER-DOT

           PERFORM VARYING WS-CHAR-IDX FROM 1 BY 1 
                   UNTIL WS-CHAR-IDX > WS-AMOUNT-LEN
               MOVE WS-AMOUNT-STR(WS-CHAR-IDX:1) TO WS-CURRENT-CHAR
               
               IF WS-CURRENT-CHAR = "."
                   ADD 1 TO WS-DOT-COUNT
                   IF WS-DOT-COUNT > 1
                       MOVE 0 TO LS-RESULT
                       MOVE "金额格式错误：多个小数点" TO LS-ERROR-MSG
                       EXIT PARAGRAPH
                   END-IF
                   MOVE 1 TO WS-AFTER-DOT
               ELSE
                   IF WS-CURRENT-CHAR NOT NUMERIC
                       MOVE 0 TO LS-RESULT
                       MOVE "金额必须是数字" TO LS-ERROR-MSG
                       EXIT PARAGRAPH
                   END-IF
                   IF WS-AFTER-DOT = 1
                       ADD 1 TO WS-DECIMAL-PLACES
                   END-IF
               END-IF
           END-PERFORM

           IF WS-DECIMAL-PLACES > 2
               MOVE 0 TO LS-RESULT
               MOVE "金额最多两位小数" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

      *>   检查金额是否大于0
           IF FUNCTION NUMVAL(WS-AMOUNT-STR) <= 0
               MOVE 0 TO LS-RESULT
               MOVE "金额必须大于 0" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO LS-RESULT.

      *> ============================================================
      *> 校验交易类型（INCOME 或 EXPENSE）
      *> ============================================================
       VALIDATE-TYPE.
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(LS-INPUT-VALUE))
                TO WS-TEMP-STRING

           IF WS-TEMP-STRING NOT = "INCOME" 
              AND WS-TEMP-STRING NOT = "EXPENSE"
               MOVE 0 TO LS-RESULT
               MOVE "类型必须是 INCOME 或 EXPENSE" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO LS-RESULT.

      *> ============================================================
      *> 校验类别（非空，长度限制）
      *> ============================================================
       VALIDATE-CATEGORY.
           IF FUNCTION LENGTH(FUNCTION TRIM(LS-INPUT-VALUE)) = 0
               MOVE 0 TO LS-RESULT
               MOVE "类别不能为空" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(LS-INPUT-VALUE)) > 50
               MOVE 0 TO LS-RESULT
               MOVE "类别长度不能超过 50 个字符" TO LS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO LS-RESULT.
