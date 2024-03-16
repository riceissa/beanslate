# beanslate – translate intuitive input to Beancount syntax

Let's say you buy some food at the grocery store using your credit card, then
you buy a book at the bookstore (also with your credit card), and then finally
you pay off your credit card using your bank account. In conventional
double-entry bookkeeping, you would do something like this:

```
Expenses:Groceries
Debit | Credit
------+-------
12.50 |

Liabilities:CreditCard
Debit | Credit
------+-------
      | 12.50
      |  8.95
21.45 |

Expenses:Entertainment
Debit | Credit
------+-------
 8.95 |

Assets:BankAccount
Debit | Credit
------+-------
      | 21.45
```


In Beancount, you would do something like this:

```
2024-03-15 "Buy some food"
  Expenses:Groceries        12.50 USD
  Liabilities:CreditCard   -12.50 USD

2024-03-15 "Buy a book"
  Expenses:Entertainment     8.95 USD
  Liabilities:CreditCard    -8.95 USD

2024-03-15 "Pay off credit card"
  Liabilities:CreditCard    21.45 USD
  Assets:BankAccount       -21.45 USD
```

With Beanslate, you can instead input the following:

```
2024-03-15 "Buy some food"
  Expenses:Groceries        12.50 USD  expense
  Liabilities:CreditCard    12.50 USD  charge

2024-03-15 "Buy a book"
  Expenses:Entertainment     8.95 USD  expense
  Liabilities:CreditCard     8.95 USD  charge

2024-03-15 "Pay off credit card"
  Liabilities:CreditCard    21.45 USD  repayment to them
  Assets:BankAccount        21.45 USD  decrease
```

Beanslate will then translate the above into an input acceptable by Beancount:

```
2024-03-15 "Buy some food"
  Expenses:Groceries        12.50  ; Debit:  USD  expense
  Liabilities:CreditCard    -12.50  ; Credit:  USD  charge

2024-03-15 "Buy a book"
  Expenses:Entertainment     8.95  ; Debit:  USD  expense
  Liabilities:CreditCard     -8.95  ; Credit:  USD  charge

2024-03-15 "Pay off credit card"
  Liabilities:CreditCard    21.45  ; Debit:  USD  repayment to them
  Assets:BankAccount        -21.45  ; Credit:  USD  decrease
```

(there's currently a bug where the currency gets placed in a comment. this will
soon be fixed)
