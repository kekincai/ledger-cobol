# Ledger - COBOL 命令行记账工具

一个使用 GnuCOBOL 编写的命令行记账工具，支持收入/支出流水录入、查询、月度报表和 CSV 导出。

## 环境依赖

- **操作系统**: macOS
- **编译器**: GnuCOBOL 3.x (`gnucobol`)
- **数据库**: SQLite3

### 安装依赖 (macOS)

```bash
brew install gnucobol sqlite3
```

## 构建方式

```bash
cd ledger-cobol
./scripts/build.sh
```

编译产物位于 `./build/bin/ledger`

## 初始化数据库

```bash
./build/bin/ledger init
```

数据库文件默认创建在 `./db/ledger.db`

## 常用命令示例

### 录入流水

```bash
# 添加收入
./build/bin/ledger add --type income --amount 30000.00 --category salary --date 2026-01-07 --note "一月工资"

# 添加支出
./build/bin/ledger add --type expense --amount 125.50 --category food --date 2026-01-07 --note "午餐拉面"
./build/bin/ledger add --type expense --amount 50.00 --category transport --date 2026-01-07
```

### 查询流水

```bash
# 列出所有流水（默认最新50条）
./build/bin/ledger list

# 按月份筛选
./build/bin/ledger list --month 2026-01

# 按日期范围筛选
./build/bin/ledger list --from 2026-01-01 --to 2026-01-31

# 按类别筛选
./build/bin/ledger list --category food

# 按类型筛选
./build/bin/ledger list --type expense

# 关键字搜索（匹配 note 或 category）
./build/bin/ledger list --q 拉面

# 分页
./build/bin/ledger list --limit 10 --offset 0
```

### 修改与删除

```bash
# 修改流水
./build/bin/ledger update --id 12 --amount 130.00 --note "修改后的备注"

# 删除流水（需确认）
./build/bin/ledger delete --id 12

# 删除流水（跳过确认）
./build/bin/ledger delete --id 12 --yes
```

### 月度报表

```bash
./build/bin/ledger report month --month 2026-01
```

输出示例：
```
Month: 2026-01
Total Income:   30000.00
Total Expense:    175.50
Net:            29824.50

Expense by Category:
- food          125.50
- transport      50.00

Income by Category:
- salary      30000.00
```

### 导出 CSV

```bash
# 导出交易明细
./build/bin/ledger export tx --month 2026-01

# 导出到指定路径
./build/bin/ledger export tx --out ./my_export.csv

# 导出月度报表
./build/bin/ledger export report --month 2026-01
```

## 返回码说明

| 返回码 | 含义 |
|--------|------|
| 0 | 成功 |
| 1 | 参数错误/校验失败 |
| 2 | 数据库错误 |
| 3 | 记录不存在（如 update/delete 的 id 不存在） |
| 4 | 文件 I/O 错误（导出失败等） |

## 数据库文件位置

数据库路径解析优先级：
1. CLI 参数：`--db path/to/ledger.db`
2. 环境变量：`LEDGER_DB`
3. 默认：`./db/ledger.db`

## 项目结构

```
ledger-cobol/
  README.md
  db/
    schema.sql          # 数据库 schema
    ledger.db           # 运行时生成
  export/               # 导出目录
  src/
    main.cbl            # CLI 解析、命令分发
    cli.cbl             # 参数解析/帮助文本
    validate.cbl        # 输入校验
    dao_tx.cbl          # 交易表 CRUD
    report.cbl          # 汇总报表逻辑
    export_csv.cbl      # CSV 输出
    common.cpy          # 常量、通用结构、错误码
    types.cpy           # 数据结构定义
  scripts/
    build.sh            # 编译脚本
    run_examples.sh     # 示例命令
  tests/
    smoke.sh            # 冒烟测试
```

## 许可证

MIT License
