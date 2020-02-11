#!/usr/bin/env node

import Kernel from './src/support/Kernel'

/*
|------------------------------------------------------------------------------
| The Launch Lug
|------------------------------------------------------------------------------
| The sole purpose of a launch lug is to provide stability for a model rocket
| prior to and through liftoff by forcing the rocket to remain parallel to
| the launch rod during the first seconds of flight, before significant
| speeds are reached and the rocket can retain stability on its own.
|
*/

(async () => {
    const command = await Kernel.findOrFail(
        process.argv.slice(2, 3)[0]
    )

    Kernel.run(command)
})()
