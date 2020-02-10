import chalk from 'chalk'
import { max } from '../support/helpers'

export default abstract class Command {
    /**
     * The name of the command.
     */
    abstract readonly name: string

    /**
     * The description of the command.
     */
    abstract readonly description: string

    /**
     * The command's options.
     */
    protected abstract readonly options: {
        [key: string]: {
            shorthand?: string,
            description: string
        }
    }

    /**
     * Execute the command.
     */
    public abstract async handle(): Promise<void>

    /**
     * Run the command.
     */
    public async run() {
        await this.handle()
    }

    /**
     * Check if this command matches the supplied signature.
     */
    public matches(name: string) {
        return this.name !== undefined && this.name === name
    }

    /**
     * Get the available options.
     */
    protected getOptions() {
        return Object.keys(this.options)
    }

    /**
     * Get an option's flags.
     */
    protected getOptionFlags(optKey: string) {
        const opt = this.options[optKey];

        const shorthand = opt.shorthand ? `-${opt.shorthand}, ` : ''
        const longhand = `--${optKey}`

        return shorthand + longhand
    }

    /**
     * Check if this command matches the supplied signature.
     */
    protected printOpt(optKey: string) {
        const opt = this.options[optKey];
        const flags = this.getOptionFlags(optKey)

        const spacing = this.makeListSpacing(
            Object.keys(this.options).map(key => this.getOptionFlags(key)),
            flags
        )

        this.line(
            this.indent() + chalk.green(flags) + spacing + opt.description
        )
    }

    /**
     * Make the proper amount of space between a list of variable length items.
     */
    protected makeListSpacing(list: string[], currentItem: string) {
        const maxSpace = 8 + max(list).length

        return ' '.repeat(maxSpace - currentItem.length)
    }

    /**
     * Check if this command matches the supplied signature.
     */
    protected indent(message: string = '') {
        return ' '.repeat(3) + message
    }

    /**
     * Print a header.
     */
    protected header(heading: string) {
        this.line(chalk.yellow(heading))
    }

    /**
     * Print a line.
     */
    protected line(message: string = '') {
        console.log(message)
    }
}
