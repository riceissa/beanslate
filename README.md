# beanslate – translate intuitive input to Beancount syntax

**WARNING:** This is just a proof of concept. It's implemented in a hacky way
using regular expressions. Not ready for real-world use.

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
  Expenses:Groceries         12.50 USD  ; Debit, Increase:   expense
  Liabilities:CreditCard    -12.50 USD  ; Credit, Increase:   charge

2024-03-15 "Buy a book"
  Expenses:Entertainment      8.95 USD  ; Debit, Increase:   expense
  Liabilities:CreditCard     -8.95 USD  ; Credit, Increase:   charge

2024-03-15 "Pay off credit card"
  Liabilities:CreditCard     21.45 USD  ; Debit, Decrease:   repayment to them
  Assets:BankAccount        -21.45 USD  ; Credit, Decrease:   decrease
```

No more having to manually think about signed amounts or what Debit/Credit mean!

Now let's say that you and your friend Bob often pay for each other's meals at
restaurants (to simplify logistics) and later PayPal each other.  Should Bob be
an Asset (accounts receivable) or a Liability (accounts payable)?  With
Beanslate, it doesn't matter! Let me show you what I mean. Let's first treat
the amount owed to Bob as a Liability:

```
2024-03-16 "Bob buys me lunch"
  Liabilities:Bob   15.00 USD  owed to them
  Expenses:Dining   15.00 USD  expense

2024-03-16 "Pay Bob back for lunch"
  Liabilities:Bob   15.00 USD  repayment to them
  Assets:PayPal     15.00 USD  spend
```

This is translated to:

```
2024-03-16 "Bob buys me lunch"
  Liabilities:Bob   -15.00 USD  ; Credit, Increase:   owed to them
  Expenses:Dining    15.00 USD  ; Debit, Increase:   expense

2024-03-16 "Pay Bob back for lunch"
  Liabilities:Bob    15.00 USD  ; Debit, Decrease:   repayment to them
  Assets:PayPal     -15.00 USD  ; Credit, Decrease:   spend
```

Now let's treat the amount Bob owes (in this case, a negative amount) as an
asset:

```
2024-03-16 "Bob buys me lunch"
  Assets:Bob        15.00 USD  owed to them
  Expenses:Dining   15.00 USD  expense

2024-03-16 "Pay Bob back for lunch"
  Assets:Bob        15.00 USD  repayment to them
  Assets:PayPal     15.00 USD  spend
```

This is translated to:

```
2024-03-16 "Bob buys me lunch"
  Assets:Bob        -15.00 USD  ; Credit, Decrease:   owed to them
  Expenses:Dining    15.00 USD  ; Debit, Increase:   expense

2024-03-16 "Pay Bob back for lunch"
  Assets:Bob         15.00 USD  ; Debit, Increase:   repayment to them
  Assets:PayPal     -15.00 USD  ; Credit, Decrease:   spend
```
