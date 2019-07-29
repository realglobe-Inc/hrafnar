const path = require("path")
const TerserPlugin = require('terser-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const mode = process.env.NODE_ENV || 'development'

module.exports = () => ({
  mode,
  entry: {
    main: path.join(__dirname, './client/index.js'),
  },
  output: {
    path: path.join(__dirname, 'dist'),
    publicPath: '/'
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: 'elm-webpack-loader',
            options: {
              optimize: true,
              },
          }
        ]
      },
      {
        test: /\.s?css$/,
        use: ['style-loader', 'css-loader', 'sass-loader']
      }
    ]
  },
  plugins: [new HtmlWebpackPlugin()],
  optimization: {
    minimizer: [
      new TerserPlugin({
        cache: true,
        parallel: true,
        terserOptions: {
          mangle: false,
          compress: {
            pure_funcs: ['F2','F3','F4','F5','F6','F7','F8','F9','A2','A3','A4','A5','A6','A7','A8','A9'],
            pure_getters: true,
            keep_fargs: false,
            unsafe_comps: true,
            unsafe: true,
          },
        },
      }),
      new TerserPlugin({
        cache: true,
        parallel: true,
        terserOptions: { mangle: true },
      }),
    ],
  },
  devServer: {
    historyApiFallback: true
  }
})
