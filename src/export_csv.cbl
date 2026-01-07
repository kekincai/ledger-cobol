      *> ============================================================
      *> export_csv.cbl - CSV 导出模块
      *> 支持导出交易明细和月度报表
      *> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXPORT-CSV.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSV-FILE ASSIGN TO WS-CSV-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CSV-FILE.
       01 CSV-RECORD               PIC X(1000).

       WORKING-STORAGE SECTION.
       COPY "common.cpy".
       COPY "types.cpy".

      *> 文件相关变量
       01 WS-CSV-FILENAME          PIC X(200) VALUE SPACES.
       01 WS-FILE-STATUS           PIC XX VALUE "00".
       01 WS-FILE-OPEN             PIC 9 VALUE 0.

      *> 时间戳（用于默认文件名）
       01 WS-CURRENT-DATE.
          05 WS-CURR-YEAR          PIC 9(4).
          05 WS-CURR-MONTH         PIC 9(2).
          05 WS-CURR-DAY           PIC 9(2).
       01 WS-CURRENT-TIME.
          05 WS-CURR-HOUR          PIC 9(2).
          05 WS-CURR-MIN           PIC 9(2).
          05 WS-CURR-SEC           PIC 9(2).
          05 WS-CURR-HSEC          PIC 9(2).

      *> CSV 行缓冲
       01 WS-CSV-LINE              PIC X(1000) VALUE SPACES.
       01 WS-CSV-FIELD             PIC X(200) VALUE SPACES.
       01 WS-ESCAPED-FIELD         PIC X(400) VALUE SPACES.

      *> 金额显示
       01 WS-AMOUNT-DISPLAY        PIC Z(9)9.99.

       PROCEDURE DIVISION.
       MAIN-EXPORT.
           STOP RUN.

      *> ============================================================
      *> 导出交易明细 CSV
      *> ============================================================
       EXPORT-TRANSACTIONS SECTION.
      *>   确定输出文件名
           IF WS-ARG-OUT-PATH NOT = SPACES
               MOVE WS-ARG-OUT-PATH TO WS-CSV-FILENAME
           ELSE
               PERFORM GENERATE-TX-FILENAME
           END-IF

      *>   打开文件
           PERFORM OPEN-CSV-FILE
           IF WS-RETURN-CODE NOT = 0
               EXIT SECTION
           END-IF

      *>   写入表头
           MOVE "ID,Date,Type,Category,Amount,Note" TO CSV-RECORD
           WRITE CSV-RECORD
           IF WS-FILE-STATUS NOT = "00"
               MOVE 4 TO WS-RETURN-CODE
               DISPLAY "错误: 写入 CSV 文件失败"
               PERFORM CLOSE-CSV-FILE
               EXIT SECTION
           END-IF

      *>   查询并写入数据行
           PERFORM WRITE-TX-ROWS

      *>   关闭文件
           PERFORM CLOSE-CSV-FILE

           IF WS-RETURN-CODE = 0
               DISPLAY "交易明细已导出到: " 
                       FUNCTION TRIM(WS-CSV-FILENAME)
           END-IF.

      *> ============================================================
      *> 导出月度报表 CSV
      *> ============================================================
       EXPORT-REPORT SECTION.
      *>   校验月份参数
           IF WS-ARG-MONTH = SPACES
               DISPLAY "错误: 必须指定 --month 参数"
               MOVE 1 TO WS-RETURN-CODE
               EXIT SECTION
           END-IF

      *>   确定输出文件名
           IF WS-ARG-OUT-PATH NOT = SPACES
               MOVE WS-ARG-OUT-PATH TO WS-CSV-FILENAME
           ELSE
               STRING "./export/report_"
                      FUNCTION TRIM(WS-ARG-MONTH)
                      ".csv"
                      DELIMITED SIZE INTO WS-CSV-FILENAME
           END-IF

      *>   打开文件
           PERFORM OPEN-CSV-FILE
           IF WS-RETURN-CODE NOT = 0
               EXIT SECTION
           END-IF

      *>   写入汇总信息
           STRING "Month," FUNCTION TRIM(WS-ARG-MONTH)
                  DELIMITED SIZE INTO CSV-RECORD
           WRITE CSV-RECORD

      *>   获取报表数据并写入
           PERFORM WRITE-REPORT-ROWS

      *>   关闭文件
           PERFORM CLOSE-CSV-FILE

           IF WS-RETURN-CODE = 0
               DISPLAY "月度报表已导出到: " 
                       FUNCTION TRIM(WS-CSV-FILENAME)
           END-IF.

      *> ============================================================
      *> 生成交易明细默认文件名
      *> ============================================================
       GENERATE-TX-FILENAME SECTION.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
                                         WS-CURRENT-TIME

           STRING "./export/transactions_"
                  WS-CURR-YEAR WS-CURR-MONTH WS-CURR-DAY "_"
                  WS-CURR-HOUR WS-CURR-MIN WS-CURR-SEC
                  ".csv"
                  DELIMITED SIZE INTO WS-CSV-FILENAME.

      *> ============================================================
      *> 打开 CSV 文件
      *> ============================================================
       OPEN-CSV-FILE SECTION.
           MOVE 0 TO WS-RETURN-CODE
           OPEN OUTPUT CSV-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 4 TO WS-RETURN-CODE
               DISPLAY "错误: 无法创建文件 " 
                       FUNCTION TRIM(WS-CSV-FILENAME)
               DISPLAY "请确保 export 目录存在"
           ELSE
               MOVE 1 TO WS-FILE-OPEN
           END-IF.

      *> ============================================================
      *> 关闭 CSV 文件
      *> ============================================================
       CLOSE-CSV-FILE SECTION.
           IF WS-FILE-OPEN = 1
               CLOSE CSV-FILE
               MOVE 0 TO WS-FILE-OPEN
           END-IF.

      *> ============================================================
      *> 写入交易数据行
      *> ============================================================
       WRITE-TX-ROWS SECTION.
      *>   实际实现需要从数据库查询数据
      *>   对每行数据调用 WRITE-TX-ROW
           CONTINUE.

      *> ============================================================
      *> 写入单条交易记录
      *> ============================================================
       WRITE-TX-ROW SECTION.
      *>   转换金额为元
           COMPUTE WS-AMOUNT-DISPLAY = WS-TX-AMOUNT-CENTS / 100

      *>   转义备注字段
           MOVE WS-TX-NOTE TO WS-CSV-FIELD
           PERFORM ESCAPE-CSV-FIELD

      *>   构建 CSV 行
           STRING WS-TX-ID ","
                  FUNCTION TRIM(WS-TX-DATE) ","
                  FUNCTION TRIM(WS-TX-TYPE) ","
                  FUNCTION TRIM(WS-TX-CATEGORY) ","
                  WS-AMOUNT-DISPLAY ","
                  FUNCTION TRIM(WS-ESCAPED-FIELD)
                  DELIMITED SIZE INTO CSV-RECORD

           WRITE CSV-RECORD.

      *> ============================================================
      *> 写入报表数据行
      *> ============================================================
       WRITE-REPORT-ROWS SECTION.
      *>   实际实现需要获取报表数据
           CONTINUE.

      *> ============================================================
      *> CSV 字段转义
      *> 处理逗号、双引号、换行符
      *> ============================================================
       ESCAPE-CSV-FIELD SECTION.
           MOVE SPACES TO WS-ESCAPED-FIELD

      *>   检查是否需要转义
           IF WS-CSV-FIELD CONTAINS "," 
              OR WS-CSV-FIELD CONTAINS '"'
              OR WS-CSV-FIELD CONTAINS X"0A"
      *>       需要用双引号包裹，内部双引号转义为 ""
               PERFORM ESCAPE-QUOTES
               STRING '"' FUNCTION TRIM(WS-ESCAPED-FIELD) '"'
                      DELIMITED SIZE INTO WS-ESCAPED-FIELD
           ELSE
               MOVE WS-CSV-FIELD TO WS-ESCAPED-FIELD
           END-IF.

      *> ============================================================
      *> 转义双引号（" -> ""）
      *> ============================================================
       ESCAPE-QUOTES SECTION.
           MOVE SPACES TO WS-ESCAPED-FIELD
           INSPECT WS-CSV-FIELD REPLACING ALL '"' BY '""'
           MOVE WS-CSV-FIELD TO WS-ESCAPED-FIELD.
