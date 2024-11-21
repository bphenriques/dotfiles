import brightness from '../../services/brightness.js';

export function Brightness() {
 return Widget.Label({
    label: brightness.bind('screen-value').as(v => `${v}`),
    setup: self => self.hook(brightness, (self, screenValue) => {
        // screenValue is the passed parameter from the 'screen-changed' signal
        self.label = screenValue ?? 0;

        // NOTE:
        // since hooks are run upon construction
        // the passed screenValue will be undefined the first time

        // all three are valid
        self.label = `${brightness.screenValue}`;
        self.label = `${brightness.screen_value}`;
        self.label = `${brightness['screen-value']}`;

    }, 'screen-changed'),
  })
}