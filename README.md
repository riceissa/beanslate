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
restaurants (to simplify logistics; some restaurants don't like it when
multiple cards are used to pay for a single table's meals)
and later PayPal each other.  Should Bob be
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

As you can see, in both cases, you just had to put "owed to them" and
"repayment to them". You no longer have to go through mental contortions like
"Money owed to Bob is a Liability, and an increase in Liabilities is a negative
amount in Beancount, so I should use a negative number" or "Bob is an Asset in
my accounting, and in this case I'm owing him money, so my Asset is going
down – so that's a negative amount".

Which keywords are supported? That part is still being figured out by me... but
at the very least, increase/decrease is supported for all account types, and
even that I think helps a lot.

## Arrow notation

The newer implementation `Beanslate.hs` actually parses the input (rather than
just doing regex search-and-replace) and supports the arrow notation suggested
by the Reddit user captain\_currie.

For example, we can parse the file [`reddit.txt`](https://github.com/riceissa/beanslate/blob/master/reddit.txt):

```bash
$ ghci Beanslate.hs
ghci> parseOrPrintErrorFromFile (some $ transaction <* some spaceChar) "reddit.txt"
([Right (Transaction {txnDate = Date {year = 2024, month = 3, day = 17},
                      txnNarration = "Buy a book",
                      txnAccountLines = [
                        TransactionAccountLine {
                          talAccountName = "Expenses:Entertainment",
                          talAmount = "8.95",
                          talSign = '+'},
                        TransactionAccountLine {
                          talAccountName = "Liabilities:CreditCard",
                          talAmount = "8.95",
                          talSign = '-'}]}),
  Right (Transaction {txnDate = Date {year = 2024, month = 3, day = 17},
                      txnNarration = "Buy a book",
                      txnAccountLines = [
                        TransactionAccountLine {
                          talAccountName = "Expenses:Entertainment",
                          talAmount = "8.95",
                          talSign = '+'},
                        TransactionAccountLine {
                          talAccountName = "Liabilities:CreditCard",
                          talAmount = "8.95",
                          talSign = '-'}]}),
  Right (Transaction {txnDate = Date {year = 2024, month = 3, day = 17},
                      txnNarration = "Buy a book",
                      txnAccountLines = [
                        TransactionAccountLine {
                          talAccountName = "Liabilities:CreditCard",
                          talAmount = "-8.95",
                          talSign = '-'},
                        TransactionAccountLine {
                          talAccountName = "Expenses:Entertainment",
                          talAmount = "8.95",
                          talSign = '+'}]}),
  Right (Transaction {txnDate = Date {year = 2024, month = 3, day = 17},
                      txnNarration = "Buy a book",
                      txnAccountLines = [
                        TransactionAccountLine {
                          talAccountName = "Liabilities:CreditCard",
                          talAmount = "-8.95",
                          talSign = '-'},
                        TransactionAccountLine {
                          talAccountName = "Expenses:Entertainment",
                          talAmount = "8.95",
                          talSign = '+'}]})],"")
```

The output is a bit messy so I've "pretty-printed" it. Hopefully you can see
that all four input styles get parsed in the same way.
