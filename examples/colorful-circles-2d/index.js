import { Elm } from './src/Main.elm';
import 'elm-canvas';


let app = Elm.Main.init({
    node: document.getElementById('elm-main'),
});
