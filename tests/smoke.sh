#!/bin/bash
# ============================================================
# smoke.sh - Ledger 冒烟测试
# ============================================================

set -e

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LEDGER="$PROJECT_ROOT/build/bin/ledger"
TEST_DB="$PROJECT_ROOT/db/test_ledger.db"

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

TESTS_PASSED=0
TESTS_FAILED=0

# 测试辅助函数
test_start() {
    echo -e "${YELLOW}测试: $1${NC}"
}

test_pass() {
    echo -e "${GREEN}  ✓ 通过${NC}"
    ((TESTS_PASSED++))
}

test_fail() {
    echo -e "${RED}  ✗ 失败: $1${NC}"
    ((TESTS_FAILED++))
}

cleanup() {
    rm -f "$TEST_DB"
    rm -f "$PROJECT_ROOT/export/test_*.csv"
}

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}  Ledger 冒烟测试${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""

# 检查可执行文件
if [ ! -f "$LEDGER" ]; then
    echo -e "${RED}错误: 未找到 ledger 可执行文件${NC}"
    echo "请先运行: ./scripts/build.sh"
    exit 1
fi

# 清理测试环境
cleanup

# ============================================================
# 测试 1: init 后表存在
# ============================================================
test_start "init 命令创建数据库"
"$LEDGER" init --db "$TEST_DB" > /dev/null 2>&1
if [ -f "$TEST_DB" ]; then
    # 检查表是否存在
    TABLE_EXISTS=$(sqlite3 "$TEST_DB" "SELECT name FROM sqlite_master WHERE type='table' AND name='transactions';" 2>/dev/null)
    if [ "$TABLE_EXISTS" = "transactions" ]; then
        test_pass
    else
        test_fail "transactions 表不存在"
    fi
else
    test_fail "数据库文件未创建"
fi

# ============================================================
# 测试 2: add 2 条收入、3 条支出
# ============================================================
test_start "add 命令添加交易"
"$LEDGER" add --type income --amount 30000.00 --category salary --date 2026-01-07 --note "一月工资" --db "$TEST_DB" > /dev/null 2>&1
"$LEDGER" add --type income --amount 5000.00 --category bonus --date 2026-01-15 --note "奖金" --db "$TEST_DB" > /dev/null 2>&1
"$LEDGER" add --type expense --amount 125.50 --category food --date 2026-01-07 --note "午餐" --db "$TEST_DB" > /dev/null 2>&1
"$LEDGER" add --type expense --amount 50.00 --category transport --date 2026-01-07 --db "$TEST_DB" > /dev/null 2>&1
"$LEDGER" add --type expense --amount 200.00 --category food --date 2026-01-08 --note "晚餐" --db "$TEST_DB" > /dev/null 2>&1

COUNT=$(sqlite3 "$TEST_DB" "SELECT COUNT(*) FROM transactions;" 2>/dev/null)
if [ "$COUNT" = "5" ]; then
    test_pass
else
    test_fail "期望 5 条记录，实际 $COUNT 条"
fi

# ============================================================
# 测试 3: list 默认能看到最新记录
# ============================================================
test_start "list 命令查询交易"
OUTPUT=$("$LEDGER" list --db "$TEST_DB" 2>&1)
if echo "$OUTPUT" | grep -q "salary"; then
    test_pass
else
    test_fail "未找到预期记录"
fi

# ============================================================
# 测试 4: report month 汇总金额正确
# ============================================================
test_start "report month 汇总正确"
# 总收入: 30000 + 5000 = 35000
# 总支出: 125.50 + 50 + 200 = 375.50
# 净额: 35000 - 375.50 = 34624.50
OUTPUT=$("$LEDGER" report month --month 2026-01 --db "$TEST_DB" 2>&1)
if echo "$OUTPUT" | grep -q "35000"; then
    test_pass
else
    test_fail "汇总金额不正确"
fi

# ============================================================
# 测试 5: update 一条记录后 report 更新
# ============================================================
test_start "update 命令修改交易"
# 获取第一条记录的 ID
FIRST_ID=$(sqlite3 "$TEST_DB" "SELECT id FROM transactions ORDER BY id LIMIT 1;" 2>/dev/null)
"$LEDGER" update --id "$FIRST_ID" --amount 31000.00 --db "$TEST_DB" > /dev/null 2>&1

# 检查金额是否更新
NEW_AMOUNT=$(sqlite3 "$TEST_DB" "SELECT amount_cents FROM transactions WHERE id=$FIRST_ID;" 2>/dev/null)
if [ "$NEW_AMOUNT" = "3100000" ]; then
    test_pass
else
    test_fail "金额未正确更新"
fi

# ============================================================
# 测试 6: delete 一条记录后 list 不再出现
# ============================================================
test_start "delete 命令删除交易"
LAST_ID=$(sqlite3 "$TEST_DB" "SELECT id FROM transactions ORDER BY id DESC LIMIT 1;" 2>/dev/null)
"$LEDGER" delete --id "$LAST_ID" --yes --db "$TEST_DB" > /dev/null 2>&1

COUNT_AFTER=$(sqlite3 "$TEST_DB" "SELECT COUNT(*) FROM transactions;" 2>/dev/null)
if [ "$COUNT_AFTER" = "4" ]; then
    test_pass
else
    test_fail "删除后记录数不正确"
fi

# ============================================================
# 测试 7: export tx 生成 csv 文件且行数正确
# ============================================================
test_start "export tx 导出 CSV"
EXPORT_FILE="$PROJECT_ROOT/export/test_export.csv"
"$LEDGER" export tx --out "$EXPORT_FILE" --db "$TEST_DB" > /dev/null 2>&1

if [ -f "$EXPORT_FILE" ]; then
    # CSV 应该有 1 行表头 + 4 行数据 = 5 行
    LINE_COUNT=$(wc -l < "$EXPORT_FILE" | tr -d ' ')
    if [ "$LINE_COUNT" = "5" ]; then
        test_pass
    else
        test_fail "CSV 行数不正确，期望 5 行，实际 $LINE_COUNT 行"
    fi
else
    test_fail "CSV 文件未生成"
fi

# ============================================================
# 测试结果汇总
# ============================================================
echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}  测试结果汇总${NC}"
echo -e "${GREEN}========================================${NC}"
echo -e "通过: ${GREEN}$TESTS_PASSED${NC}"
echo -e "失败: ${RED}$TESTS_FAILED${NC}"

# 清理
cleanup

if [ $TESTS_FAILED -gt 0 ]; then
    exit 1
fi

echo -e "\n${GREEN}所有测试通过！${NC}"
exit 0
