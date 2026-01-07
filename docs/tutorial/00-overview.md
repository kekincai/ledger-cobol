# COBOL 实战教程：从零构建命令行记账工具

## 教程概述

本教程将带你从零开始，使用 COBOL 语言构建一个完整的命令行记账工具 `ledger`。通过这个项目，你将学习：

1. **COBOL 基础语法** - 程序结构、数据定义、过程逻辑
2. **命令行参数处理** - 解析用户输入的命令和选项
3. **与 C 语言互操作** - 调用 C 函数访问 SQLite 数据库
4. **实际项目开发** - 代码组织、错误处理、测试

## 为什么选择这个项目？

记账工具是一个"麻雀虽小，五脏俱全"的项目：
- 有数据输入（add 命令）
- 有数据查询（list 命令）
- 有数据修改和删除（update/delete 命令）
- 有数据汇总（report 命令）
- 有文件输出（export 命令）

这些功能覆盖了大多数业务系统的核心操作。

## 开发环境

- **操作系统**: macOS（Linux 也适用）
- **COBOL 编译器**: GnuCOBOL 3.x
- **数据库**: SQLite3
- **C 编译器**: GCC 或 Clang

### 安装依赖（macOS）

```bash
# 安装 GnuCOBOL
brew install gnucobol

# 安装 SQLite（macOS 通常已预装）
brew install sqlite3

# 验证安装
cobc --version
sqlite3 --version
```

## 项目结构

```
ledger-cobol/
├── README.md                 # 项目说明
├── docs/
│   └── tutorial/            # 本教程
├── db/
│   ├── schema.sql           # 数据库表结构
│   └── ledger.db            # 运行时生成的数据库文件
├── export/                  # CSV 导出目录
├── src/
│   ├── main.cbl             # 主程序（COBOL）
│   ├── sqlite_wrapper.c     # SQLite 包装函数（C）
│   └── sqlite_wrapper.h     # C 头文件
├── scripts/
│   └── build.sh             # 编译脚本
└── tests/
    └── smoke.sh             # 冒烟测试
```

## 教程目录

1. [COBOL 程序基础结构](01-cobol-basics.md)
2. [数据定义与变量](02-data-division.md)
3. [过程逻辑与控制流](03-procedure-division.md)
4. [命令行参数解析](04-cli-parsing.md)
5. [COBOL 调用 C 函数](05-cobol-c-interop.md)
6. [SQLite 数据库操作](06-sqlite-operations.md)
7. [完整功能实现](07-full-implementation.md)
8. [编译与测试](08-build-and-test.md)

## 最终效果预览

```bash
# 初始化数据库
./ledger init

# 添加收入
./ledger add --type income --amount 30000 --category salary --date 2026-01-07

# 添加支出
./ledger add --type expense --amount 125.50 --category food --date 2026-01-07

# 查看流水
./ledger list

# 输出：
# ID        DATE        TYPE     CATEGORY     AMOUNT       NOTE
# --------- ----------  -------  ----------  -----------  ----------
#         2  2026-01-07  EXPENSE  food         125.50  
#         1  2026-01-07  INCOME   salary     30000.00  

# 生成月度报表
./ledger report month --month 2026-01
```

让我们开始吧！
