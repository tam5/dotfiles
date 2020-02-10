import glob from 'glob'
import yargs from 'yargs'

class Kernel {
    /**
     * Handle an incoming command.
     */
    handle() {

        console.log('handle something', this.commands())
    }

    /**
     * Get all available commands.
     */
    commands() {
        return glob.sync('./src/commands/**/*.js')
    }
}

export default new Kernel()
