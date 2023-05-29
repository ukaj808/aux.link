const path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = {
  mode: 'production',
  entry: {
    room: ['./static/scripts/aux-audio-player.js', 
	    './static/scripts/room-main.js',
	    './static/scripts/order-element.js',
	    './static/scripts/room-message-listener.js',
	    './static/scripts/currently-playing-element.js'
        ],
    home: ['./static/scripts/home-main.js']
  },
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: '[name]_bundle.js',
  },
   plugins: [
    // ...
    new CopyWebpackPlugin({
      patterns: [
        {
          from: 'static/scripts/workers/*.js',
          to: '[name][ext]',
        },
        {
          from: 'static/styles/*.css',
          to: '[name][ext]',
        },
        {
          from: 'static/images/*.*',
          to: '[name][ext]',

        },
      ],
    }),
  ],
};
