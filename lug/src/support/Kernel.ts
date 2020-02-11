import glob from 'tiny-glob'
import yargs from 'yargs'
import List from '../commands/List'
import Command from '../commands/Command'
import { errorBlock } from './console'

export default class Kernel {
    /**
     * Handle an incoming command.
     */
    public static async findOrFail(name: string) {
        const commands = await this.commands()
        const found = commands.find(command => command.matches(name))

        if (found) {
            return found
        }

        const addendum = name ? ` '${name}'` : ''
        errorBlock('Invalid Command' + addendum + '!')

        await this.run(new List())
        process.exit(2)
    }

    /**
     * Run a command.
     */
    public static async run(command: Command) {
        await command.run(this.register(command))
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

    /**
     * Register the commands. We don't use yargs directly so we can have
     * some more control, but yargs does offer some awesome features
     * that we want to take advantage of, like auto-completions.
     */
    private static register(command: Command) {
        return yargs.command(command.name, command.description, builder => {
            const options = command.getOptions()

            Object.values(options).forEach((opt: any) => {
                opt.alias = opt.shorthand
                opt.describe = opt.description
            })

            builder.options(options)
        }).help(false).argv
    }
}
