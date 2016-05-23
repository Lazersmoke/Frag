var scene = new THREE.Scene();
var camera = new THREE.PerspectiveCamera( 75, window.innerWidth/window.innerHeight, 0.1, 1000 );

var renderer = new THREE.WebGLRenderer();
renderer.setSize( window.innerWidth, window.innerHeight );
document.body.appendChild( renderer.domElement );

var material = new THREE.MeshBasicMaterial( { color: 0x00ff00, wireframe:true } );

camera.position.z = 5;

var render = function () {
  requestAnimationFrame( render );

  renderer.render(scene, camera);
};

render();

sock = new WebSocket("ws://potatobox.no-ip.info:9160")
sock.onopen = function(e){
  console.log("Socket Opened")
}
sock.onclose = function(e){
}
sock.onmessage = function(e){
  console.log(e.data)
  parsed = JSON.parse(e.data)
  objectList = parsed.players.concat(parsed.objects)
  scene.children = []
  objectList.forEach(function(a){
    var newbox = new THREE.BoxGeometry(a.size.x,a.size.y,a.size.z)
    var newcube = new THREE.Mesh(newbox, material)
    newcube.position.x = a.pos.x
    newcube.position.y = a.pos.y
    newcube.position.z = a.pos.z
    scene.add(newcube)
  })
}
 
var $obj = function(pos, size){
  this.pos = pos
  this.size = size
}
var $point = function(x, y, z){
  this.x = x
  this.y = y
  this.z = z
}
document.getElementById("ready").onclick = function(){
  sock.send("Ready")
}
document.getElementById("connect").onclick = function(){
  sock.send("asdf")
}

document.onkeydown = keyDownHandler
document.onkeyup = keyUpHandler

var keystates = {87: false,65: false, 83: false,68: false}
function keyDownHandler(e){
  console.log("Down: " + e.keyCode)
  if(!keystates[e.keyCode]){
    keystates[e.keyCode] = true
    switch(e.keyCode){
      case 87:
        sock.send("+forward")
        break;
      case 65:
        sock.send("+left")
        break;
      case 83:
        sock.send("+back")
        break;
      case 68:
        sock.send("+right")
        break;
    }
  }
}
function keyUpHandler(e){
  console.log("Up: " + e.keyCode)
  keystates[e.keyCode] = false
  switch(e.keyCode){
    case 87:
      sock.send("-forward")
      break;
    case 65:
      sock.send("-left")
      break;
    case 83:
      sock.send("-back")
      break;
    case 68:
      sock.send("-right")
      break;
  }
}
