      *> ============================================================
      *> cli.cbl - 命令行参数解析与帮助文本
      *> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "common.cpy".
       COPY "types.cpy".

      *> 参数解析工作变量
       01 WS-ARG-COUNT             PIC 9(3) VALUE 0.
       01 WS-ARG-IDX               PIC 9(3) VALUE 0.
       01 WS-CURRENT-ARG           PIC X(200) VALUE SPACES.
       01 WS-NEXT-ARG              PIC X(200) VALUE SPACES.
       01 WS-ARG-LOWER             PIC X(200) VALUE SPACES.

      *> 解析后的参数
       01 WS-PARSED-ARGS.
          05 WS-ARG-COMMAND        PIC X(20) VALUE SPACES.
          05 WS-ARG-SUBCOMMAND     PIC X(20) VALUE SPACES.
          05 WS-ARG-TYPE           PIC X(7)  VALUE SPACES.
          05 WS-ARG-AMOUNT         PIC X(20) VALUE SPACES.
          05 WS-ARG-CATEGORY       PIC X(50) VALUE SPACES.
          05 WS-ARG-DATE           PIC X(10) VALUE SPACES.
          05 WS-ARG-NOTE           PIC X(200) VALUE SPACES.
          05 WS-ARG-ID             PIC 9(10) VALUE 0.
          05 WS-ARG-FROM           PIC X(10) VALUE SPACES.
          05 WS-ARG-TO             PIC X(10) VALUE SPACES.
          05 WS-ARG-MONTH          PIC X(7)  VALUE SPACES.
          05 WS-ARG-KEYWORD        PIC X(50) VALUE SPACES.
          05 WS-ARG-LIMIT          PIC 9(4)  VALUE 50.
          05 WS-ARG-OFFSET         PIC 9(8)  VALUE 0.
          05 WS-ARG-DB-PATH        PIC X(200) VALUE SPACES.
          05 WS-ARG-OUT-PATH       PIC X(200) VALUE SPACES.
          05 WS-ARG-YES-FLAG       PIC 9     VALUE 0.

       PROCEDURE DIVISION.
       MAIN-CLI.
           PERFORM PARSE-ARGUMENTS
           STOP RUN.

      *> ============================================================
      *> 解析命令行参数
      *> ============================================================
       PARSE-ARGUMENTS.
           ACCEPT WS-ARG-COUNT FROM ARGUMENT-NUMBER

           IF WS-ARG-COUNT = 0
               PERFORM SHOW-HELP
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   获取主命令
           MOVE 1 TO WS-ARG-IDX
           ACCEPT WS-CURRENT-ARG FROM ARGUMENT-VALUE
           MOVE FUNCTION LOWER-CASE(FUNCTION TRIM(WS-CURRENT-ARG))
                TO WS-ARG-COMMAND

      *>   解析后续参数
           ADD 1 TO WS-ARG-IDX
           PERFORM UNTIL WS-ARG-IDX > WS-ARG-COUNT
               ACCEPT WS-CURRENT-ARG FROM ARGUMENT-VALUE
               MOVE FUNCTION LOWER-CASE(FUNCTION TRIM(WS-CURRENT-ARG))
                    TO WS-ARG-LOWER

               EVALUATE TRUE
      *>           类型参数
                   WHEN WS-ARG-LOWER = "--type"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION UPPER-CASE(
                                FUNCTION TRIM(WS-NEXT-ARG))
                                TO WS-ARG-TYPE
                       END-IF

      *>           金额参数
                   WHEN WS-ARG-LOWER = "--amount"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-AMOUNT
                       END-IF

      *>           类别参数
                   WHEN WS-ARG-LOWER = "--category"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-CATEGORY
                       END-IF

      *>           日期参数
                   WHEN WS-ARG-LOWER = "--date"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-DATE
                       END-IF

      *>           备注参数
                   WHEN WS-ARG-LOWER = "--note"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-NOTE
                       END-IF

      *>           ID 参数
                   WHEN WS-ARG-LOWER = "--id"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION NUMVAL(
                                FUNCTION TRIM(WS-NEXT-ARG))
                                TO WS-ARG-ID
                       END-IF

      *>           起始日期
                   WHEN WS-ARG-LOWER = "--from"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-FROM
                       END-IF

      *>           结束日期
                   WHEN WS-ARG-LOWER = "--to"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-TO
                       END-IF

      *>           月份参数
                   WHEN WS-ARG-LOWER = "--month"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-MONTH
                       END-IF

      *>           关键字搜索
                   WHEN WS-ARG-LOWER = "--q"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-KEYWORD
                       END-IF

      *>           分页限制
                   WHEN WS-ARG-LOWER = "--limit"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION NUMVAL(
                                FUNCTION TRIM(WS-NEXT-ARG))
                                TO WS-ARG-LIMIT
                       END-IF

      *>           分页偏移
                   WHEN WS-ARG-LOWER = "--offset"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION NUMVAL(
                                FUNCTION TRIM(WS-NEXT-ARG))
                                TO WS-ARG-OFFSET
                       END-IF

      *>           数据库路径
                   WHEN WS-ARG-LOWER = "--db"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-DB-PATH
                       END-IF

      *>           输出路径
                   WHEN WS-ARG-LOWER = "--out"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-OUT-PATH
                       END-IF

      *>           确认标志
                   WHEN WS-ARG-LOWER = "--yes"
                       MOVE 1 TO WS-ARG-YES-FLAG

      *>           子命令（如 report month, export tx）
                   WHEN OTHER
                       IF WS-ARG-SUBCOMMAND = SPACES
                           MOVE FUNCTION LOWER-CASE(
                                FUNCTION TRIM(WS-CURRENT-ARG))
                                TO WS-ARG-SUBCOMMAND
                       END-IF
               END-EVALUATE

               ADD 1 TO WS-ARG-IDX
           END-PERFORM.

      *> ============================================================
      *> 显示帮助信息
      *> ============================================================
       SHOW-HELP.
           DISPLAY "Ledger - COBOL 命令行记账工具"
           DISPLAY " "
           DISPLAY "用法: ledger <command> [options]"
           DISPLAY " "
           DISPLAY "命令:"
           DISPLAY "  init                    初始化数据库"
           DISPLAY "  add                     新增交易流水"
           DISPLAY "  list                    查询交易列表"
           DISPLAY "  update                  修改交易"
           DISPLAY "  delete                  删除交易"
           DISPLAY "  report month            生成月度报表"
           DISPLAY "  export tx               导出交易明细 CSV"
           DISPLAY "  export report           导出月度报表 CSV"
           DISPLAY "  help                    显示帮助信息"
           DISPLAY " "
           DISPLAY "add 选项:"
           DISPLAY "  --type income|expense   交易类型（必填）"
           DISPLAY "  --amount 123.45         金额（必填）"
           DISPLAY "  --category food         类别（必填）"
           DISPLAY "  --date 2026-01-07       日期（必填）"
           DISPLAY "  --note '备注'           备注（可选）"
           DISPLAY " "
           DISPLAY "list 选项:"
           DISPLAY "  --from YYYY-MM-DD       起始日期"
           DISPLAY "  --to YYYY-MM-DD         结束日期"
           DISPLAY "  --month YYYY-MM         月份"
           DISPLAY "  --category xxx          类别"
           DISPLAY "  --type income|expense   类型"
           DISPLAY "  --q keyword             关键字搜索"
           DISPLAY "  --limit N               每页数量（默认50）"
           DISPLAY "  --offset M              偏移量（默认0）"
           DISPLAY " "
           DISPLAY "update 选项:"
           DISPLAY "  --id N                  交易ID（必填）"
           DISPLAY "  --type/--amount/--category/--date/--note"
           DISPLAY " "
           DISPLAY "delete 选项:"
           DISPLAY "  --id N                  交易ID（必填）"
           DISPLAY "  --yes                   跳过确认"
           DISPLAY " "
           DISPLAY "report 选项:"
           DISPLAY "  --month YYYY-MM         月份（必填）"
           DISPLAY " "
           DISPLAY "通用选项:"
           DISPLAY "  --db path               数据库路径"
           DISPLAY "  --out path              输出文件路径"
           DISPLAY " "
           DISPLAY "返回码:"
           DISPLAY "  0 - 成功"
           DISPLAY "  1 - 参数错误"
           DISPLAY "  2 - 数据库错误"
           DISPLAY "  3 - 记录不存在"
           DISPLAY "  4 - 文件I/O错误".
