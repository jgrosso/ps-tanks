// https://github.com/ethul/purescript-webpack-example/blob/master/webpack.config.js

'use strict';

const path = require('path');
const webpack = require('webpack');

const isWebpackDevServer = process.argv
  .filter(a => path.basename(a) === 'webpack-dev-server')
  .length;

const isWatch = process.argv
  .filter(a => a === '--watch')
  .length;

const plugins =
  (isWebpackDevServer || !isWatch)
    ? []
    : [
      function() {
        this.plugin('done', (stats) => {
          process.stderr.write(stats.toString('errors-only'));
        });
      }
    ];

module.exports = {
  devtool: 'eval-source-map',

  mode: 'development',

  entry: './src/index.js',

  output: {
    filename: 'bundle.js',
    path: __dirname,
    pathinfo: true
  },

  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              src: [
                'bower_components/purescript-*/src/**/*.purs',
                'src/**/*.purs'
              ],
              bundle: false,
              psc: 'psa',
              watch: isWebpackDevServer || isWatch,
              pscIde: false
            }
          }
        ]
      }
    ]
  },

  resolve: {
    modules: ['node_modules', 'bower_components'],
    extensions: ['.purs', '.js']
  },

  plugins: [
    ...plugins,
    new webpack.LoaderOptionsPlugin({
      debug: true
    })
  ]
};
