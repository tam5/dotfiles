import chalk from 'chalk'
import Command from './Command'
import Kernel from '../support/Kernel'

export default class List extends Command {
    /**
     * The name of the command.
     */
    public readonly name = 'list';

    /**
     * The description of the command.
     */
    public readonly description = 'List the available commands'

    /**
     * Execute the command.
     */
    public async handle() {
        this.header('Usage:')
        this.line(this.indent('command [options]'))
        this.line()

        this.header('Options:')
        this.getOptionKeys().forEach(opt => this.printOpt(opt))
        this.line()

        this.header('Available Commands:')
        const commands = await Kernel.commands()

        const buckets: {
            [key: string]: Command[]
        } = {}

        commands.forEach(command => {
            const bucket = command.name.includes(':') ? command.name.split(':')[0] : ''
            buckets[bucket] = [].concat(command)
        })

        Object.keys(buckets).forEach(bucket => {
            if (bucket !== '') {
                this.header(' ' + bucket)
            }

            buckets[bucket].forEach(command => {
                const spacing = this.makeListSpacing(commands.map(c => c.name), command.name)

                this.line(
                    this.indent() + chalk.green(command.name) + spacing + chalk.white(command.description)
                )
            })
        })
    }
}
