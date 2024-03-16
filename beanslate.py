#!/usr/bin/env python3

import re
import sys

def figure_out_sign(account_type, transaction_keyword):
    try:
        if account_type == "Assets":
            return {
                    "increase": "+",
                    "decrease": "-",
                    "paid": "-",
                    "owed to me": "+",
                    "received": "+",
                    "repayment to me": "-",
                    "repayment to them": "+",
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
            "repayment to them", "payment"]
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
        if figure_out_sign(account_type, transaction_keyword) == "-":
            print(re.sub(r"(\d+\.\d+)", r"-\1  ;", line), end="")
        else:
            print(re.sub(r"(\d+\.\d+)", r"\1  ;", line), end="")
    else:
        print(line, end="")
