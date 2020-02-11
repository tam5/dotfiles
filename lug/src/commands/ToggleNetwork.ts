import inquirer from 'inquirer'
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
     *
     * Based on https://apple.stackexchange.com/questions/238958/easily-disable-enable-all-network-connectivity
     * we are using different locations as a hack to enable saving different network profiles.
     */
    public async handle() {
        const current = this.execOrDie('networksetup -getcurrentlocation').trim()

        const choices = this.execOrDie('networksetup -listlocations').trim().split('\n').map(profile => {
            return profile === current ? `${profile} (current)` : profile
        })

        const answer = await inquirer.prompt({
            name: 'profile',
            type: 'list',
            message: 'Please specify the network profile to use:',
            choices: choices
        })

        this.execOrDie(`networksetup -switchtolocation ${answer.profile}`)
    }
}
