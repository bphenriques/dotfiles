class BrightnessService extends Service {
    static {
        Service.register(this, { 'screen-changed': ['float'] }, { 'screen-value': ['float', 'rw'] });
    }

    #interface = Utils.exec("sh -c 'ls -w1 /sys/class/backlight | head -1'");
    #screenValue = 0;
    #max = Number(Utils.exec('brightnessctl max'));

    // the getter/setter has to be in snake_case
    get screen_value() {
        return this.#screenValue;
    }

    set screen_value(percent) {
        if (percent < 0)
            percent = 0;
        if (percent > 1)
            percent = 1;

        Utils.execAsync(`brightnessctl set ${percent * 100}% -q`);
    }

    constructor() {
        super();
        const brightness = `/sys/class/backlight/${this.#interface}/brightness`;
        Utils.monitorFile(brightness, () => this.#onChange());
        this.#onChange();
    }

    #onChange() {
        this.#screenValue = Number(Utils.exec('brightnessctl get')) / this.#max;
        this.changed('screen-value');
        this.emit('screen-changed', this.#screenValue);
    }

    // overwriting the connect method, let's you
    // change the default event that widgets connect to
    connect(event = 'screen-changed', callback) {
        return super.connect(event, callback);
    }
}

// Singleton
const service = new BrightnessService;
export default service;