import * as Path from 'path';

const entry = './src/App.bs.js';

const output = {
    filename: 'main.js',
    path: Path.resolve('dist'),
};

const stats = 'errors-only';

export default {
    entry,
    output,
    stats,
};
