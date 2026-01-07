#!/bin/bash
# ============================================================
# build.sh - Ledger COBOL 项目编译脚本
# ============================================================

set -e

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC_DIR="$PROJECT_ROOT/src"
BUILD_DIR="$PROJECT_ROOT/build"
BIN_DIR="$BUILD_DIR/bin"
OBJ_DIR="$BUILD_DIR/obj"

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

# SQLite 路径
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

echo -e "\n${YELLOW}Compiling C wrapper...${NC}"
gcc -c -fPIC $SQLITE_INC \
    "$SRC_DIR/sqlite_wrapper.c" \
    -o "$OBJ_DIR/sqlite_wrapper.o"

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
echo "  $BIN_DIR/ledger add --type expense --amount 100 --category food --date 2026-01-07"
