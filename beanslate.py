#!/usr/bin/env python3

import re
import sys

SIGN_MAP = {
    "Assets": {
        "increase": "+",
        "decrease": "-",
        "opening balance": "+",
        "paid": "-",  # e.g. Assets:PayPal
        "spend": "-",  # e.g. Assets:PayPal
        "used": "-",  # e.g. Assets:Cash
        "owed to me": "+",  # e.g. Assets:Bob
        "owed to them": "-",  # e.g. Assets:Bob
        "received": "+",  # e.g. Assets:PayPal
        "receive": "+",  # e.g. Assets:PayPal
        "repayment to me": "-",  # e.g. Assets:Bob
        "repayment to them": "+",  # e.g. Assets:Bob
    },
    "Liabilities": {
        "increase": "-",
        "decrease": "+",
        "owed to me": "+",  # e.g. Liabilities:Bob
        "owed to them": "-",  # e.g. Liabilities:Bob
        "charge": "-",  # e.g. Liabilities:CreditCard
        "payment": "+",  # e.g. Liabilities:CreditCard
        "repayment to me": "-",  # e.g. Liabilities:Bob
        "repayment to them": "+",  # e.g. Liabilities:Bob
    },
    "Equity": {
        "increase": "-",
        "decrease": "+",
        "opening balance": "-",
    },
    "Income": {
        "increase": "-",
        "decrease": "+",
        "owed to me": "-",
        "earned": "-",  # e.g. Income:Salary
        "income": "-",  # e.g. Income:Salary
    },
    "Expenses": {
        "increase": "+",
        "decrease": "-",
        "spent": "+",  # e.g. Expenses:Groceries
        "expense": "+",  # e.g. Expenses:Groceries
        "rebate": "-",
    }
}

def figure_out_debit_credit(sign):
    if sign == "+":
        return "Debit"
    if sign == "-":
        return "Credit"
    raise ValueError("The sign must be + or -")

def figure_out_increase_decrease(account_type, sign):
    if account_type == "Assets":
        return "Increase" if figure_out_debit_credit(sign) == "Debit" else "Decrease"
    if account_type == "Liabilities":
        return "Decrease" if figure_out_debit_credit(sign) == "Debit" else "Increase"
    if account_type == "Equity":
        return "Decrease" if figure_out_debit_credit(sign) == "Debit" else "Increase"
    if account_type == "Income":
        return "Decrease" if figure_out_debit_credit(sign) == "Debit" else "Increase"
    if account_type == "Expenses":
        return "Increase" if figure_out_debit_credit(sign) == "Debit" else "Decrease"
    raise ValueError("Unknown account type!")

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
        increase_or_decrease = figure_out_increase_decrease(account_type, sign)
        if sign == "-":
            print(re.sub(r"(\d+\.\d+)(\s+)([A-Z]+)", fr"-\1\2\3  ; {debit_or_credit}, {increase_or_decrease}: ", line),
                  end="")
        else:
            print(re.sub(r"(\d+\.\d+)(\s+)([A-Z]+)", fr" \1\2\3  ; {debit_or_credit}, {increase_or_decrease}: ", line),
                  end="")
    else:
        print(line, end="")
