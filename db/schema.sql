-- Ledger 数据库 Schema
-- 交易流水表

CREATE TABLE IF NOT EXISTS transactions (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    tx_date     TEXT    NOT NULL,   -- YYYY-MM-DD
    tx_type     TEXT    NOT NULL,   -- INCOME | EXPENSE
    category    TEXT    NOT NULL,
    amount_cents INTEGER NOT NULL,  -- amount * 100 (分)
    note        TEXT,
    created_at  TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

-- 索引
CREATE INDEX IF NOT EXISTS idx_tx_date ON transactions(tx_date);
CREATE INDEX IF NOT EXISTS idx_tx_type ON transactions(tx_type);
CREATE INDEX IF NOT EXISTS idx_tx_category ON transactions(category);
