# Lambda Calculus Interpreter

### Part A - Lambda Calculus

- Church Numeral Encoding
- Fresh/Used variable tracking using an infinite list of variables
- Capture-avoiding substitution
- Two methods of beta reduction to normal form
    - Normal order reduction (leftmost outermost)
    - Applicative order reduction (leftmost innermost)

### Part B - Abstract Machines

These machines are more practical and efficient reduction strategies than using beta reduction directly.

- Partial Abstract Machine (PAM)
- Krivine Abstract Machine (KAM)

## Notes

This was written as Functional Programming coursework in 2019. The KAM was incomplete. I returned to the project in 2026 and completed it.

## Possible Future Plans:
- [X] fix readback for KAM
- [ ] add a repl
- [ ] implement a CEK machine
