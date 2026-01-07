# 第三章：过程逻辑与控制流

## 3.1 过程部的结构

过程部（PROCEDURE DIVISION）包含程序的执行逻辑，由段落（Paragraph）和节（Section）组成：

```cobol
       PROCEDURE DIVISION.
       
       MAIN-PROGRAM.                    *> 段落名
           PERFORM INIT-DATA
           PERFORM PROCESS-DATA
           PERFORM CLEANUP
           STOP RUN.
       
       INIT-DATA.                       *> 另一个段落
           MOVE 0 TO WS-COUNTER
           MOVE SPACES TO WS-NAME.
       
       PROCESS-DATA.
           ...
```

## 3.2 基本语句

### 3.2.1 DISPLAY - 输出

```cobol
       DISPLAY "Hello, World!"
       DISPLAY "Name: " WS-NAME
       DISPLAY "Count = " WS-COUNT " items"
       
      *> 不换行输出
       DISPLAY "Enter name: " WITH NO ADVANCING
```

### 3.2.2 ACCEPT - 输入

```cobol
      *> 从控制台读取
       ACCEPT WS-NAME FROM CONSOLE
       
      *> 从命令行参数读取
       ACCEPT WS-ARG-COUNT FROM ARGUMENT-NUMBER
       ACCEPT WS-CURRENT-ARG FROM ARGUMENT-VALUE
       
      *> 从环境变量读取
       ACCEPT WS-DB-PATH FROM ENVIRONMENT "LEDGER_DB"
```

### 3.2.3 MOVE - 赋值

```cobol
       MOVE 100 TO WS-COUNTER
       MOVE "John" TO WS-NAME
       MOVE SPACES TO WS-BUFFER          *> 清空字符串
       MOVE ZEROS TO WS-AMOUNT           *> 清零
       
      *> 多目标赋值
       MOVE 0 TO WS-A WS-B WS-C
```

### 3.2.4 COMPUTE - 计算

```cobol
      *> 简单计算
       COMPUTE WS-TOTAL = WS-PRICE * WS-QUANTITY
       
      *> 复杂表达式
       COMPUTE WS-RESULT = (WS-A + WS-B) / 2 * 1.1
       
      *> 使用内置函数
       COMPUTE WS-LENGTH = FUNCTION LENGTH(WS-NAME)
       COMPUTE WS-UPPER = FUNCTION UPPER-CASE(WS-NAME)
```

### 3.2.5 算术语句

```cobol
       ADD 1 TO WS-COUNTER
       ADD WS-A WS-B GIVING WS-C         *> WS-C = WS-A + WS-B
       
       SUBTRACT 10 FROM WS-BALANCE
       SUBTRACT WS-A FROM WS-B GIVING WS-C
       
       MULTIPLY WS-PRICE BY WS-QTY GIVING WS-TOTAL
       
       DIVIDE WS-TOTAL BY 100 GIVING WS-RESULT
       DIVIDE WS-A BY WS-B GIVING WS-Q REMAINDER WS-R
```

## 3.3 条件判断

### 3.3.1 IF 语句

```cobol
      *> 基本 IF
       IF WS-AGE >= 18
           DISPLAY "Adult"
       END-IF
       
      *> IF-ELSE
       IF WS-BALANCE > 0
           DISPLAY "Positive"
       ELSE
           DISPLAY "Non-positive"
       END-IF
       
      *> IF-ELSE IF-ELSE
       IF WS-SCORE >= 90
           DISPLAY "A"
       ELSE IF WS-SCORE >= 80
           DISPLAY "B"
       ELSE IF WS-SCORE >= 70
           DISPLAY "C"
       ELSE
           DISPLAY "F"
       END-IF
```

### 3.3.2 条件表达式

```cobol
      *> 比较运算符
       IF WS-A = WS-B           *> 等于
       IF WS-A NOT = WS-B       *> 不等于
       IF WS-A > WS-B           *> 大于
       IF WS-A >= WS-B          *> 大于等于
       IF WS-A < WS-B           *> 小于
       IF WS-A <= WS-B          *> 小于等于
       
      *> 也可以用英文单词
       IF WS-A EQUAL TO WS-B
       IF WS-A GREATER THAN WS-B
       IF WS-A LESS THAN OR EQUAL TO WS-B
       
      *> 逻辑运算符
       IF WS-A > 0 AND WS-B > 0
       IF WS-A > 0 OR WS-B > 0
       IF NOT (WS-A = 0)
       
      *> 字符串判断
       IF WS-NAME = SPACES      *> 是否全空格
       IF WS-NAME NOT = SPACES  *> 是否非空
       
      *> 数字判断
       IF WS-COUNT NUMERIC      *> 是否是数字
       IF WS-AMOUNT POSITIVE    *> 是否为正
       IF WS-AMOUNT NEGATIVE    *> 是否为负
       IF WS-AMOUNT ZERO        *> 是否为零
```

### 3.3.3 EVALUATE 语句（类似 switch）

```cobol
       EVALUATE TRUE
           WHEN WS-COMMAND = "init"
               PERFORM CMD-INIT
           WHEN WS-COMMAND = "add"
               PERFORM CMD-ADD
           WHEN WS-COMMAND = "list"
               PERFORM CMD-LIST
           WHEN WS-COMMAND = "help"
               PERFORM SHOW-HELP
           WHEN OTHER
               DISPLAY "Unknown command"
               MOVE 1 TO WS-RETURN-CODE
       END-EVALUATE
       
      *> 也可以直接判断变量
       EVALUATE WS-STATUS
           WHEN 0
               DISPLAY "Success"
           WHEN 1
               DISPLAY "Warning"
           WHEN 2 THRU 9
               DISPLAY "Error"
           WHEN OTHER
               DISPLAY "Unknown"
       END-EVALUATE
```

## 3.4 循环

### 3.4.1 PERFORM UNTIL

```cobol
      *> 基本循环（先判断后执行）
       MOVE 1 TO WS-I
       PERFORM UNTIL WS-I > 10
           DISPLAY "Count: " WS-I
           ADD 1 TO WS-I
       END-PERFORM
       
      *> 先执行后判断
       PERFORM WITH TEST AFTER UNTIL WS-I > 10
           DISPLAY "Count: " WS-I
           ADD 1 TO WS-I
       END-PERFORM
```

### 3.4.2 PERFORM VARYING（类似 for 循环）

```cobol
      *> 从 1 到 10
       PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
           DISPLAY "Number: " WS-I
       END-PERFORM
       
      *> 从 10 到 1（倒序）
       PERFORM VARYING WS-I FROM 10 BY -1 UNTIL WS-I < 1
           DISPLAY "Countdown: " WS-I
       END-PERFORM
```

### 3.4.3 PERFORM TIMES

```cobol
      *> 执行固定次数
       PERFORM 5 TIMES
           DISPLAY "Hello"
       END-PERFORM
       
      *> 使用变量
       PERFORM WS-COUNT TIMES
           DISPLAY "Repeat"
       END-PERFORM
```

## 3.5 段落调用

### 3.5.1 PERFORM 段落

```cobol
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INIT-DATA           *> 调用段落
           PERFORM PROCESS-DATA
           STOP RUN.
       
       INIT-DATA.
           MOVE 0 TO WS-COUNTER.       *> 段落以句号结束
       
       PROCESS-DATA.
           DISPLAY "Processing...".
```

### 3.5.2 EXIT PARAGRAPH

```cobol
       VALIDATE-INPUT.
           IF WS-NAME = SPACES
               DISPLAY "Name is required"
               MOVE 1 TO WS-ERROR
               EXIT PARAGRAPH           *> 提前退出段落
           END-IF
           
           IF WS-AGE < 0
               DISPLAY "Invalid age"
               MOVE 1 TO WS-ERROR
               EXIT PARAGRAPH
           END-IF
           
           MOVE 0 TO WS-ERROR.
```

## 3.6 字符串操作

### 3.6.1 STRING - 连接

```cobol
       STRING WS-FIRST-NAME DELIMITED SPACE
              " " DELIMITED SIZE
              WS-LAST-NAME DELIMITED SPACE
              INTO WS-FULL-NAME
       END-STRING
```

### 3.6.2 UNSTRING - 分割

```cobol
       UNSTRING WS-DATE DELIMITED BY "-"
           INTO WS-YEAR WS-MONTH WS-DAY
       END-UNSTRING
```

### 3.6.3 INSPECT - 检查/替换

```cobol
      *> 计数
       INSPECT WS-TEXT TALLYING WS-COUNT FOR ALL "A"
       
      *> 替换
       INSPECT WS-TEXT REPLACING ALL "," BY "."
```

### 3.6.4 内置函数

```cobol
      *> 去除空格
       MOVE FUNCTION TRIM(WS-NAME) TO WS-TRIMMED
       
      *> 大小写转换
       MOVE FUNCTION UPPER-CASE(WS-NAME) TO WS-UPPER
       MOVE FUNCTION LOWER-CASE(WS-NAME) TO WS-LOWER
       
      *> 长度
       COMPUTE WS-LEN = FUNCTION LENGTH(WS-NAME)
       COMPUTE WS-LEN = FUNCTION LENGTH(FUNCTION TRIM(WS-NAME))
       
      *> 字符串转数字
       COMPUTE WS-NUM = FUNCTION NUMVAL(WS-STR)
       
      *> 当前日期时间
       MOVE FUNCTION CURRENT-DATE TO WS-DATETIME
```

## 3.7 本项目中的实际应用

### 命令分发逻辑

```cobol
       MAIN-PROGRAM.
           PERFORM PARSE-ARGUMENTS      *> 解析命令行
           PERFORM RESOLVE-DB-PATH      *> 确定数据库路径

           EVALUATE TRUE
               WHEN WS-COMMAND = "init"
                   PERFORM CMD-INIT
               WHEN WS-COMMAND = "add"
                   PERFORM CMD-ADD
               WHEN WS-COMMAND = "list"
                   PERFORM CMD-LIST
               WHEN WS-COMMAND = "help"
                   PERFORM SHOW-HELP
               WHEN OTHER
                   PERFORM SHOW-HELP
                   MOVE 1 TO WS-RETURN-CODE
           END-EVALUATE

           CALL "db_close" RETURNING WS-RC
           MOVE WS-RETURN-CODE TO RETURN-CODE
           STOP RUN.
```

### 参数校验逻辑

```cobol
       CMD-ADD.
      *>   校验必填参数
           IF WS-ARG-TYPE = SPACES
               DISPLAY "Error: --type is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
           IF WS-ARG-AMOUNT = SPACES
               DISPLAY "Error: --amount is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   校验类型值
           IF WS-ARG-TYPE NOT = "INCOME" 
              AND WS-ARG-TYPE NOT = "EXPENSE"
               DISPLAY "Error: type must be INCOME or EXPENSE"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
           ...
```

### 循环读取数据

```cobol
       CMD-LIST.
           ...
           
      *>   循环输出结果
           MOVE 0 TO WS-RC
           PERFORM UNTIL WS-RC NOT = 0
               CALL "tx_list_next"
                    USING BY REFERENCE WS-LIST-ID
                          BY REFERENCE WS-LIST-DATE
                          ...
                    RETURNING WS-RC
               IF WS-RC = 0
                   COMPUTE WS-AMOUNT-DISP = WS-LIST-AMOUNT / 100
                   DISPLAY WS-LIST-ID "  " 
                           FUNCTION TRIM(WS-LIST-DATE) "  "
                           ...
               END-IF
           END-PERFORM
           
           CALL "tx_list_end" RETURNING WS-RC.
```

## 3.8 练习

1. 写一个程序，接受用户输入的数字，判断是正数、负数还是零

2. 写一个程序，计算 1 到 100 的和

3. 写一个程序，接受用户输入的字符串，统计其中字母 'a' 的个数

## 下一章预告

下一章我们将学习如何解析命令行参数，这是构建 CLI 工具的关键技能。
