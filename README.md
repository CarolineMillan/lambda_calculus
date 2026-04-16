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

## How to run

```
ghci fpcw.hs
```
to run in the interactive repl.

```numeral <integer>``` returns the church encoding for ```<integer>```.

```variables``` is an infinite list of variables

```substitute x n m``` performs capture-avoiding substitution (```M[N/x]```).

```beta x``` and ```normalize x``` perform normal order reduction (single step and finding normal form, respectively).

```aBeta x``` and ```aNormalize x``` perform applicative order reduction (single step and finding normal form, respectively).

The file contains examples ```state2```, ```state3``` and ```state4```.

```p_run <state>``` runs the PAM on a given state

```p_readback <state>``` prints a Term from a given State using the PAM.

```run <state>``` runs the KAM on a given state

```readback <state>``` prints a Term from a given State using the KAM.

## Notes

This was written as Functional Programming coursework in 2019. The KAM was incomplete. I returned to the project in 2026 and completed it.

<!--

## Possible Future Plans:
- [X] fix readback for KAM
- [ ] add a repl
- [ ] implement a CEK machine

-->
