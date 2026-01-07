# 第八章：编译与测试

## 8.1 编译流程

### 8.1.1 编译步骤

1. **编译 C 代码**：将 `sqlite_wrapper.c` 编译为目标文件
2. **编译 COBOL 代码**：将 `main.cbl` 编译并链接 C 目标文件和 SQLite 库

### 8.1.2 完整的构建脚本

```bash
#!/bin/bash
# scripts/build.sh

set -e  # 遇到错误立即退出

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC_DIR="$PROJECT_ROOT/src"
BUILD_DIR="$PROJECT_ROOT/build"
BIN_DIR="$BUILD_DIR/bin"
OBJ_DIR="$BUILD_DIR/obj"

# 颜色输出
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}  Ledger COBOL Build${NC}"
echo -e "${GREEN}========================================${NC}"

# 检查依赖
if ! command -v cobc &> /dev/null; then
    echo -e "${RED}Error: GnuCOBOL (cobc) not found${NC}"
    echo "Install: brew install gnucobol"
    exit 1
fi

if ! command -v sqlite3 &> /dev/null; then
    echo -e "${RED}Error: SQLite3 not found${NC}"
    echo "Install: brew install sqlite3"
    exit 1
fi

echo -e "${YELLOW}GnuCOBOL:${NC} $(cobc --version | head -1)"

# 创建目录
mkdir -p "$BIN_DIR" "$OBJ_DIR"
mkdir -p "$PROJECT_ROOT/db" "$PROJECT_ROOT/export"

# SQLite 路径（macOS Homebrew）
if [ -d "/opt/homebrew/opt/sqlite" ]; then
    SQLITE_INC="-I/opt/homebrew/opt/sqlite/include"
    SQLITE_LIB="-L/opt/homebrew/opt/sqlite/lib"
elif [ -d "/usr/local/opt/sqlite" ]; then
    SQLITE_INC="-I/usr/local/opt/sqlite/include"
    SQLITE_LIB="-L/usr/local/opt/sqlite/lib"
else
    SQLITE_INC=""
    SQLITE_LIB=""
fi

# 编译 C 包装函数
echo -e "\n${YELLOW}Compiling C wrapper...${NC}"
gcc -c -fPIC $SQLITE_INC \
    "$SRC_DIR/sqlite_wrapper.c" \
    -o "$OBJ_DIR/sqlite_wrapper.o"

# 编译 COBOL 并链接
echo -e "${YELLOW}Compiling COBOL...${NC}"
cobc -x -free -o "$BIN_DIR/ledger" \
    "$SRC_DIR/main.cbl" \
    "$OBJ_DIR/sqlite_wrapper.o" \
    $SQLITE_LIB -lsqlite3

echo -e "\n${GREEN}========================================${NC}"
echo -e "${GREEN}  Build Complete!${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo "Binary: $BIN_DIR/ledger"
echo ""
echo "Usage:"
echo "  $BIN_DIR/ledger help"
echo "  $BIN_DIR/ledger init"
```

### 8.1.3 编译参数说明

**GCC 参数：**
- `-c`：只编译，不链接
- `-fPIC`：生成位置无关代码（Position Independent Code）
- `-I`：指定头文件搜索路径
- `-o`：指定输出文件

**GnuCOBOL 参数：**
- `-x`：生成可执行文件
- `-free`：使用自由格式
- `-o`：指定输出文件
- `-L`：指定库文件搜索路径
- `-l`：链接指定的库

## 8.2 测试

### 8.2.1 手动测试

```bash
# 清理并初始化
rm -f ./db/ledger.db
./build/bin/ledger init

# 添加测试数据
./build/bin/ledger add --type income --amount 30000 --category salary --date 2026-01-07
./build/bin/ledger add --type expense --amount 125.50 --category food --date 2026-01-07
./build/bin/ledger add --type expense --amount 50 --category transport --date 2026-01-07

# 验证数据
./build/bin/ledger list
./build/bin/ledger report month --month 2026-01
```

### 8.2.2 自动化冒烟测试

```bash
#!/bin/bash
# tests/smoke.sh

set -e

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LEDGER="$PROJECT_ROOT/build/bin/ledger"
TEST_DB="$PROJECT_ROOT/db/test_ledger.db"

# 颜色
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

TESTS_PASSED=0
TESTS_FAILED=0

test_start() {
    echo -e "${YELLOW}Test: $1${NC}"
}

test_pass() {
    echo -e "${GREEN}  ✓ Passed${NC}"
    ((TESTS_PASSED++))
}

test_fail() {
    echo -e "${RED}  ✗ Failed: $1${NC}"
    ((TESTS_FAILED++))
}

cleanup() {
    rm -f "$TEST_DB"
}

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}  Ledger Smoke Tests${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""

# 检查可执行文件
if [ ! -f "$LEDGER" ]; then
    echo -e "${RED}Error: ledger not found${NC}"
    echo "Run: ./scripts/build.sh"
    exit 1
fi

# 清理
cleanup

# ============================================================
# Test 1: init 创建数据库
# ============================================================
test_start "init creates database"
"$LEDGER" init --db "$TEST_DB" > /dev/null 2>&1
if [ -f "$TEST_DB" ]; then
    TABLE_EXISTS=$(sqlite3 "$TEST_DB" "SELECT name FROM sqlite_master WHERE type='table' AND name='transactions';" 2>/dev/null)
    if [ "$TABLE_EXISTS" = "transactions" ]; then
        test_pass
    else
        test_fail "transactions table not found"
    fi
else
    test_fail "database file not created"
fi

# ============================================================
# Test 2: add 添加交易
# ============================================================
test_start "add creates transactions"
"$LEDGER" add --type income --amount 30000 --category salary --date 2026-01-07 --db "$TEST_DB" > /dev/null 2>&1
"$LEDGER" add --type expense --amount 125.50 --category food --date 2026-01-07 --db "$TEST_DB" > /dev/null 2>&1
"$LEDGER" add --type expense --amount 50 --category transport --date 2026-01-07 --db "$TEST_DB" > /dev/null 2>&1

COUNT=$(sqlite3 "$TEST_DB" "SELECT COUNT(*) FROM transactions;" 2>/dev/null)
if [ "$COUNT" = "3" ]; then
    test_pass
else
    test_fail "expected 3 records, got $COUNT"
fi

# ============================================================
# Test 3: list 显示交易
# ============================================================
test_start "list shows transactions"
OUTPUT=$("$LEDGER" list --db "$TEST_DB" 2>&1)
if echo "$OUTPUT" | grep -q "salary"; then
    test_pass
else
    test_fail "expected record not found"
fi

# ============================================================
# Test 4: report 汇总正确
# ============================================================
test_start "report calculates correctly"
OUTPUT=$("$LEDGER" report month --month 2026-01 --db "$TEST_DB" 2>&1)
# 总收入应该是 30000.00
if echo "$OUTPUT" | grep -q "30000"; then
    test_pass
else
    test_fail "income total incorrect"
fi

# ============================================================
# Test 5: delete 删除交易
# ============================================================
test_start "delete removes transaction"
"$LEDGER" delete --id 3 --yes --db "$TEST_DB" > /dev/null 2>&1
COUNT=$(sqlite3 "$TEST_DB" "SELECT COUNT(*) FROM transactions;" 2>/dev/null)
if [ "$COUNT" = "2" ]; then
    test_pass
else
    test_fail "expected 2 records after delete, got $COUNT"
fi

# ============================================================
# 结果汇总
# ============================================================
echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}  Test Results${NC}"
echo -e "${GREEN}========================================${NC}"
echo -e "Passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Failed: ${RED}$TESTS_FAILED${NC}"

cleanup

if [ $TESTS_FAILED -gt 0 ]; then
    exit 1
fi

echo -e "\n${GREEN}All tests passed!${NC}"
exit 0
```

### 8.2.3 运行测试

```bash
chmod +x tests/smoke.sh
./tests/smoke.sh
```

输出示例：
```
========================================
  Ledger Smoke Tests
========================================

Test: init creates database
  ✓ Passed
Test: add creates transactions
  ✓ Passed
Test: list shows transactions
  ✓ Passed
Test: report calculates correctly
  ✓ Passed
Test: delete removes transaction
  ✓ Passed

========================================
  Test Results
========================================
Passed: 5
Failed: 0

All tests passed!
```

## 8.3 调试技巧

### 8.3.1 COBOL 调试输出

```cobol
      *> 添加调试输出
       CMD-ADD.
           DISPLAY "DEBUG: type=" WS-ARG-TYPE
           DISPLAY "DEBUG: amount=" WS-ARG-AMOUNT
           DISPLAY "DEBUG: category=" WS-ARG-CATEGORY
           ...
```

### 8.3.2 C 调试输出

```c
int tx_add(...) {
    fprintf(stderr, "DEBUG: date=[%s]\n", date_buf);
    fprintf(stderr, "DEBUG: type=[%s]\n", type_buf);
    fprintf(stderr, "DEBUG: amount=%ld\n", amount_cents);
    ...
}
```

### 8.3.3 SQLite 调试

```bash
# 直接查看数据库内容
sqlite3 ./db/ledger.db

# 常用命令
.tables              # 列出所有表
.schema transactions # 查看表结构
SELECT * FROM transactions;  # 查看所有数据
```

### 8.3.4 GnuCOBOL 调试选项

```bash
# 编译时启用调试信息
cobc -x -free -g -debug main.cbl ...

# 运行时启用跟踪
COB_SET_TRACE=Y ./ledger list
```

## 8.4 常见问题

### 8.4.1 "Cannot open database"

**原因：** 数据库路径不正确或目录不存在

**解决：**
```bash
mkdir -p ./db
./ledger init
```

### 8.4.2 "no such table: transactions"

**原因：** 数据库未初始化

**解决：**
```bash
./ledger init
```

### 8.4.3 COBOL 字符串有尾部空格

**原因：** COBOL 字符串是固定长度的，用空格填充

**解决：** 在 C 端使用 `trim_string` 函数处理

### 8.4.4 数值类型不匹配

**原因：** COBOL 的 `COMP` 与 C 的 `int` 字节序不同

**解决：** 使用 `COMP-5` 而不是 `COMP`

## 8.5 部署

### 8.5.1 创建发布包

```bash
#!/bin/bash
# scripts/package.sh

VERSION="1.0.0"
PACKAGE_NAME="ledger-$VERSION"
PACKAGE_DIR="dist/$PACKAGE_NAME"

mkdir -p "$PACKAGE_DIR"

# 复制可执行文件
cp build/bin/ledger "$PACKAGE_DIR/"

# 复制文档
cp README.md "$PACKAGE_DIR/"

# 创建必要目录
mkdir -p "$PACKAGE_DIR/db"
mkdir -p "$PACKAGE_DIR/export"

# 打包
cd dist
tar -czvf "$PACKAGE_NAME.tar.gz" "$PACKAGE_NAME"
echo "Package created: dist/$PACKAGE_NAME.tar.gz"
```

### 8.5.2 安装

```bash
# 解压
tar -xzvf ledger-1.0.0.tar.gz

# 移动到 PATH
sudo cp ledger-1.0.0/ledger /usr/local/bin/

# 或者添加到 PATH
export PATH="$PATH:/path/to/ledger-1.0.0"
```

## 8.6 总结

恭喜你完成了这个 COBOL 实战教程！你已经学会了：

1. **COBOL 基础** - 程序结构、数据定义、过程逻辑
2. **命令行开发** - 参数解析、帮助信息、返回码
3. **C 互操作** - 调用 C 函数、数据类型映射
4. **数据库操作** - SQLite 增删改查
5. **项目工程** - 编译脚本、自动化测试

这些技能不仅适用于 COBOL，也是通用的软件开发能力。

## 8.7 进一步学习

1. **COBOL 高级特性**
   - 文件处理（顺序文件、索引文件）
   - 报表生成
   - 子程序和模块化

2. **数据库**
   - 嵌入式 SQL（ESQL）
   - 事务处理
   - 存储过程

3. **企业级开发**
   - CICS 事务处理
   - JCL 作业控制
   - 大型机环境

## 附录：完整项目结构

```
ledger-cobol/
├── README.md
├── docs/
│   └── tutorial/
│       ├── 00-overview.md
│       ├── 01-cobol-basics.md
│       ├── 02-data-division.md
│       ├── 03-procedure-division.md
│       ├── 04-cli-parsing.md
│       ├── 05-cobol-c-interop.md
│       ├── 06-sqlite-operations.md
│       ├── 07-full-implementation.md
│       └── 08-build-and-test.md
├── db/
│   ├── schema.sql
│   └── ledger.db (运行时生成)
├── export/
├── src/
│   ├── main.cbl
│   ├── sqlite_wrapper.c
│   └── sqlite_wrapper.h
├── scripts/
│   ├── build.sh
│   └── run_examples.sh
├── tests/
│   └── smoke.sh
└── build/
    ├── bin/
    │   └── ledger
    └── obj/
        └── sqlite_wrapper.o
```

感谢学习本教程！
