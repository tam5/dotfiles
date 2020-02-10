import chalk from 'chalk'
import glob from 'tiny-glob'
import List from '../commands/List'
import { errorBlock } from './console'

export default class Kernel {
    /**
     * Handle an incoming command.
     */
    public static async findOrFail(name: string) {
        const found = (await this.commands()).find(command => command.matches(name))

        if (found) {
            return found
        }

        const addendum = name ? ` '${name}'` : ''
        errorBlock('Invalid Command' + addendum + '!')
        await (new List()).run()
        process.exit(2)
    }

    /**
     * Get the available commands.
     */
    public static async commands() {
        // Travers the 'Commands' dir to find the available commands
        const imports = (await glob('src/commands/**/*.ts'))
            .map(file => file.replace('.ts', ''))
            .map(async file => await import('../../' + file))

        // Dynamically import the commands
        const commands = (await Promise.all(imports)).map(mod => mod.default)

        // Create command instances
        return commands.map(command => new command())
            .filter(command => command.name)
    }
}
