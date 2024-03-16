#!/usr/bin/env python3

import re
import sys

def figure_out_debit_credit(sign):
    return "Debit" if sign == "+" else "Credit"

def figure_out_sign(account_type, transaction_keyword):
    try:
        if account_type == "Assets":
            return {
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
                    # "payment": "-",
                    }[transaction_keyword]
        if account_type == "Liabilities":
            return {
                    "increase": "-",
                    "decrease": "+",
                    "owed to me": "+",
                    "charge": "-",
                    "payment": "+",
                    "repayment to them": "+",
                    }[transaction_keyword]
        if account_type == "Equity":
            return {
                    "opening balance": "-",
                    }[transaction_keyword]
        if account_type == "Income":
            return {
                    "owed to me": "-",
                    "earned": "-",
                    }[transaction_keyword]
        if account_type == "Expenses":
            return {
                    "spent": "+",
                    "rebate": "-",
                    }[transaction_keyword]
    except KeyError:
        print(f"Can't use {transaction_keyword} for {account_type} accounts!",
              file=sys.stderr)
        sys.exit()

for line in sys.stdin:
    account_types = ["Assets", "Liabilities", "Equity", "Income", "Expenses"]
    account_types_re = "|".join(account_types)
    transaction_keywords = ["spent", "charge", "paid", "decrease",
            "increase", "owed to me", "owed to them", "rebate", "earned",
            "received", "repaid to me", "repaid to them", "repayment to me",
            "repayment to them", "payment", "opening balance"]
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
        debit_or_credit = figure_out_debit_credit(sign)
        if sign == "-":
            print(re.sub(r"(\d+\.\d+)", fr"-\1  ; {debit_or_credit}: ", line), end="")
        else:
            print(re.sub(r"(\d+\.\d+)", fr"\1  ; {debit_or_credit}: ", line), end="")
    else:
        print(line, end="")
