const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
  entry: {
    app: "./src/app/app.js",
    embed: "./src/app/embed.js",
  },
  plugins: [
    new HtmlWebpackPlugin({
      filename: "index.html",
      template: "src/index.html",
      chunks: ["app"],
    }),
    new HtmlWebpackPlugin({
      filename: "embed.html",
      template: "src/embed.html",
      chunks: ["embed"],
    }),
    new HtmlWebpackPlugin({
      filename: "register.html",
      template: "src/register.html",
      chunks: [],
    }),
  ],
  output: {
    filename: "[name].bundle.js",
    path: path.resolve(__dirname, "..", "www"),
  },
  //mode: 'development',
  module: {
    rules: [
      {
        test: /\.js$/,
        use: {
          loader: "babel-loader",
          options: {
            sourceMaps: "both",
            presets: ["@babel/preset-env"],
            plugins: ["@babel/plugin-proposal-class-properties"],
          },
        },
      },
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
    ],
  },
};
