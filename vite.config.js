import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";

export default defineConfig({
  plugins: [scalaJSPlugin()],
  build: {
    sourcemap: false,
    lib: {
      entry: 'visualizations-server/target/scala-3.4.2/visualizations-server-opt/main.js',
      name: 'AnimatedGrid',
      fileName: 'animated-grid'
    }
  }
});
