import Command from './Command'

export default class ToggleNetwork extends Command {
    /**
     * The name of the command.
     */
    public readonly name = 'toggle:network';

    /**
     * The description of the command.
     */
    public readonly description = 'Toggle all network access on or off'

    /**
     * Execute the command.
     */
    public async handle() {
        //
    }
}
