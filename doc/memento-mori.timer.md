# Timers

The `memento-mori.timer` (nickname `mori-timer`) package provides some
useful functions for scheduling asynchronous (interrupting) function calls,
message sends, and exits.

There are two kinds of timers. Delays (suffixed with `-after`) execute code
after a certain amount of time has passed, and then never execute
again. Intervals (suffixed with `-interval`) execute regularly with a
certain delay between executions until cancelled.

Timer-scheduling functions return `timer` objects which may be used to
cancel their delayed or repeated execution.

## `mori-timer` dictionary

#### *[function]* `call-after time function`

Calls `function` after `time` seconds have elapsed. `call-after` will
interrupt the current thread in order to execute `function`'s code.

#### *[function]* `call-interval interval function &key (delay 0)`

Calls `function` every `interval` seconds until the returned timer is
cancelled. `delay` is the number of seconds to wait before the first
call.

#### *[function]* `cancel-timer timer`

Given a `timer` returned by one of the `mori-timer` functions, unschedules
that timer and prevents it from executing in the future.

#### *[function]* `send-after time message &key (actor (current-actor))`

Sends `message` to `actor` after `time` seconds have elapsed.

#### *[function]* `send-interval interval message &key (actor (current-actor)) (delay 0)`
 
Sends `message` to `actor` every `interval` seconds until the returned
timer is cancelled. `delay` is the number of seconds to wait before the
first message is sent.

#### *[function]* `exit-after time reason &optional (actor (current-actor))`

Calls `mori:exit` on `actor` with reason `reason` after `time` seconds have
elapsed.

#### *[function]* `kill-after time &optional (actor (current-actor))`

Kills `actor` after `time` seconds have elapsed.
