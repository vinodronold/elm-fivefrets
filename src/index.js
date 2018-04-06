import { Main } from './Main.elm'
// import InitJS from './App/JS'
import registerServiceWorker from './registerServiceWorker'

var app = Main.embed(document.getElementById('root'))

// app.ports.dataToJS.subscribe(msg => {
//   switch (msg.tag) {
//     case 'TestDataToJS':
//       console.log('TestDataToJS')
//       break

//     default:
//       break
//   }
// })

// app.ports.dataFromJS.send({ tag: 'TestFromJS', data: null })

// InitJS()

registerServiceWorker()
