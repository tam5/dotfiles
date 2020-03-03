import chalk from 'chalk'
import { max } from '../support/helpers'
const execAsync = require('child_process').exec;
const execSync = require('child_process').execSync;

interface Options {
    [key: string]: {
        shorthand?: string,
        description: string
    }
}

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
    protected readonly options?: Options

    /**
     * The command's options.
     */
    private readonly defaultOptions: Options = {
        help: {
            shorthand: 'h',
            description: 'Show this help message'
        }
    }

    /**
     * The parsed arguments.
     */
    protected argv: any;

    /**
     * Execute the command.
     */
    public abstract async handle(): Promise<void>

    /**
     * Run the command.
     */
    public async run(argv: any) {
        this.argv = argv

        if (this.getOption('help')) {
            this.help()
            process.exit()
        }

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
    public getOptions() {
        return { ...this.defaultOptions, ...this.options }
    }

    /**
     * Get the available options.
     */
    protected getOption(key: string) {
        return this.argv[key]
    }

    /**
     * Get the available options.
     */
    protected getOptionKeys() {
        return Object.keys(this.getOptions())
    }

    /**
     * Get an option's flags.
     */
    protected getOptionFlags(optKey: string) {
        const opt = this.getOptions()[optKey];

        const shorthand = opt.shorthand ? `-${opt.shorthand}, ` : ''
        const longhand = `--${optKey}`

        return shorthand + longhand
    }

    /**
     * Check if this command matches the supplied signature.
     */
    protected printOpt(optKey: string) {
        const opt = this.getOptions()[optKey];
        const flags = this.getOptionFlags(optKey)

        const spacing = this.makeListSpacing(
            Object.keys(this.getOptions()).map(key => this.getOptionFlags(key)),
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
     * Print a success message.
     */
    protected success(message: string = '') {
        this.line(chalk.green('🎗 ' + message))
    }

    /**
     * Print a line.
     */
    protected line(message: string = '') {
        console.log(message)
    }

    /**
     * Show the help for this command.
     */
    protected help() {
        this.header('Description:')
        this.line(this.indent(this.description))
        this.line()

        this.header('Usage:')
        this.line(this.indent(this.name + ' [options]'))
        this.line()

        this.header('Options:')
        this.getOptionKeys().forEach(opt => this.printOpt(opt))
    }

    /**
     * Execute a shell command.
     */
    protected execOrDie(command: string): string {
        try {
            return execSync(command, { stdio: 'pipe' }).toString()
        } catch (e) {
            console.error(chalk.red('Failed to execute command:'))
            console.error(this.indent() + command)
            console.error(e)
            process.exit(1)
        }
    }
    /**
     * Execute a shell command.
     */
    protected execAsyncOrDie(command: string): string {
        try {
            return execAsync(command, { stdio: 'pipe' }).toString()
        } catch (e) {
            console.error(chalk.red('Failed to execute command:'))
            console.error(this.indent() + command)
            console.error(e)
            process.exit(1)
        }
    }
}
