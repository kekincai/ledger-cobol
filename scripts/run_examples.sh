#!/bin/bash
# ============================================================
# run_examples.sh - Ledger 示例命令演示
# ============================================================

set -e

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LEDGER="$PROJECT_ROOT/build/bin/ledger"

# 颜色输出
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

run_cmd() {
    echo -e "${CYAN}$ $@${NC}"
    "$@" || true
    echo ""
}

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}  Ledger 示例命令演示${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""

# 检查可执行文件
if [ ! -f "$LEDGER" ]; then
    echo "错误: 未找到 ledger 可执行文件"
    echo "请先运行: ./scripts/build.sh"
    exit 1
fi

echo -e "${YELLOW}1. 显示帮助${NC}"
run_cmd "$LEDGER" help

echo -e "${YELLOW}2. 初始化数据库${NC}"
run_cmd "$LEDGER" init

echo -e "${YELLOW}3. 添加收入${NC}"
run_cmd "$LEDGER" add --type income --amount 30000.00 --category salary --date 2026-01-07 --note "一月工资"

echo -e "${YELLOW}4. 添加支出${NC}"
run_cmd "$LEDGER" add --type expense --amount 125.50 --category food --date 2026-01-07 --note "午餐拉面"
run_cmd "$LEDGER" add --type expense --amount 50.00 --category transport --date 2026-01-07 --note "地铁"
run_cmd "$LEDGER" add --type expense --amount 200.00 --category food --date 2026-01-06 --note "晚餐"

echo -e "${YELLOW}5. 查询所有流水${NC}"
run_cmd "$LEDGER" list

echo -e "${YELLOW}6. 按月份查询${NC}"
run_cmd "$LEDGER" list --month 2026-01

echo -e "${YELLOW}7. 按类别查询${NC}"
run_cmd "$LEDGER" list --category food

echo -e "${YELLOW}8. 生成月度报表${NC}"
run_cmd "$LEDGER" report month --month 2026-01

echo -e "${YELLOW}9. 导出交易明细 CSV${NC}"
run_cmd "$LEDGER" export tx --month 2026-01

echo -e "${YELLOW}10. 导出月度报表 CSV${NC}"
run_cmd "$LEDGER" export report --month 2026-01

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}  示例演示完成${NC}"
echo -e "${GREEN}========================================${NC}"
