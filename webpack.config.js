const path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = {
  mode: 'production',
  devtool: 'source-map',
  resolve: {
    extensions: ["", ".webpack.js", ".web.js", ".ts", ".tsx", ".js"],
  },
  externals: {
    flickity: 'Flickity',
  },
  module: {
    rules: [
      // All files with a '.ts' or '.tsx' extension will be handled by 'ts-loader'.
      { test: /\.tsx?$/, loader: "ts-loader" },
      // All output '.js' files will have any sourcemaps re-processed by 'source-map-loader'.
      { test: /\.js$/, loader: "source-map-loader" },
      {
        test: /\.(?:js|mjs|cjs)$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: [
              ['@babel/preset-env', { targets: "defaults" }]
            ]
          }
        }
      }
    ],
  },
  entry: {
    room: ['./static/scripts/room-main.ts'],
    audio_socket_worker: ['./static/scripts/aux-audio-socket-worker.ts'],
    audio_worklet_processor: ['./static/scripts/aux-worklet-processor.ts'],
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
