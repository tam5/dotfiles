import chalk from 'chalk'

/**
 * Print an error messsage to the console in a 'block' style.
 */
export function errorBlock(message: string) {
    const padding = ' '
    const content = pad(message, padding)

    console.error()
    console.error(chalk.bgRed(padding.repeat(content.length)))
    console.error(chalk.bgRed(content))
    console.error(chalk.bgRed(padding.repeat(content.length)))
    console.error()
}

/**
 * Pad a string.
 */
function pad(message: string, delim = ' ', size = 8) {
    const padding = delim.repeat(size)

    return padding + message + padding
}
