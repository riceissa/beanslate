#!/usr/bin/env python3

import re
import sys

SIGN_MAP = {
    "Assets": {
        "increase": "+",
        "decrease": "-",
        "paid": "-",
        "spend": "-",
        "owed to me": "+",
        "received": "+",
        "receive": "+",
        "repayment to me": "-",
        "repayment to them": "+",
        "opening balance": "+",
    },
    "Liabilities": {
        "increase": "-",
        "decrease": "+",
        "owed to me": "+",
        "charge": "-",
        "payment": "+",
        "repayment to them": "+",
    },
    "Equity": {
        "opening balance": "-",
    },
    "Income": {
        "owed to me": "-",
        "earned": "-",
        "income": "-",
    },
    "Expenses": {
        "spent": "+",
        "expense": "+",
        "rebate": "-",
    }
}

def figure_out_debit_credit(sign):
    return "Debit" if sign == "+" else "Credit"

def figure_out_sign(account_type, transaction_keyword):
    try:
        return SIGN_MAP[account_type][transaction_keyword]
    except KeyError:
        print(f"Can't use {transaction_keyword} for {account_type} accounts!",
              file=sys.stderr)
        sys.exit()

for line in sys.stdin:
    account_types = list(SIGN_MAP.keys())
    account_types_re = "|".join(account_types)
    transaction_keywords = []
    for v in SIGN_MAP.values():
        transaction_keywords.extend(v.keys())
    m = re.match(rf"\s+({account_types_re}):", line)
    if m:
        account_type = m.group(1)
        line_ = line.split(";")[0].rstrip()
        transaction_keyword = None
        for word in transaction_keywords:
            if line_.endswith(" " + word):
                transaction_keyword = word
        if transaction_keyword is None:
            print(f"Cannot find any transaction keyword on this line: {line}",
                  file=sys.stderr, end="")
            sys.exit()
        sign = figure_out_sign(account_type, transaction_keyword)
        assert sign in ["+", "-"]
        debit_or_credit = figure_out_debit_credit(sign)
        if sign == "-":
            print(re.sub(r"(\d+\.\d+)", fr"-\1  ; {debit_or_credit}: ", line),
                  end="")
        else:
            print(re.sub(r"(\d+\.\d+)", fr"\1  ; {debit_or_credit}: ", line),
                  end="")
    else:
        print(line, end="")
