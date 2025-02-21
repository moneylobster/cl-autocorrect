## cl-autocorrect

Suggests corrections to your misspelled functions. Can be quickloaded from Ultralisp as "autocorrect". `fuzzy-match` is a dependency.

### Setup
#### SLIME/Sly
Call the following function in your `~/.swank.lisp` (for SLIME) or `~/.slynk.lisp` (for Sly) file. (You should also load this system beforehand: e.g. `(ql:quickload "autocorrect")`)

SLIME:
```lisp
(autocorrect:slime-install-autocorrect)
```
Sly:
```lisp
(autocorrect:sly-install-autocorrect)
```
You will see a "Autocorrect hook installed!" among the other startup output in your inferior-lisp buffer.

It's not possible (to my knowledge) to install/remove this hook without restarting SLIME/Sly.
#### Command-line
It's enough to set the `*debugger-hook*` for the autocorrect to take effect. You can also add this to your `.sbclrc` (or equivalent) file to have it load on startup.
```lisp
(setf *debugger-hook* #'autocorrect:autocorrecting-debugger)
```
### Usage
Once the hook is in place, when you type in a misspelled function call you should see an autocorrect suggestion like this:
```lisp
(caf '(1 2))
=>
The function COMMON-LISP-USER::CAF is undefined.
   [Condition of type UNDEFINED-FUNCTION]

Restarts:
 0: [AUTOCORRECT] Replace the function with CAR.
 1: [CONTINUE] Retry calling CAF.
 2: [USE-VALUE] Call specified function.
 3: [RETURN-VALUE] Return specified values.
 4: [RETURN-NOTHING] Return zero values.
 ...
 ```
Pressing 0 here will replace `caf` with `car`, returning 1 as the result.

### Configuration

See the `*correction-mode*` variable.

### Improvements/TODOs

- Maybe provide multiple autocorrect suggestions?
