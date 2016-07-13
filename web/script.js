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

sock = new WebSocket("ws://localhost:9160")
sock.onopen = function(e){
  console.log("Socket Opened")
  document.getElementById("connect").disabled = ''
  document.getElementById("ready").disabled = ''
}
sock.onclose = function(e){}
sock.onmessage = function(e){
  //console.log(e.data)
  parsed = JSON.parse(e.data)
  objectList = parsed.players.concat(parsed.objects)

  camera.position.x = parsed.players[0].pos.x
  camera.position.y = parsed.players[0].pos.y + 2
  camera.position.z = parsed.players[0].pos.z

  camera.lookAt(new THREE.Vector3(
        camera.position.x + parsed.players[0].dir.x,
        camera.position.y + parsed.players[0].dir.y,
        camera.position.z + parsed.players[0].dir.z))

  scene.children = []
  planeList = 
  [
    [new $point(-5,-2,-5)
    ,new $point(-5,-2, 5)
    ,new $point( 5,-2, 5)
    ]
  , [new $point(-5,-2,-5)
    ,new $point( 5,-2, 5)
    ,new $point( 5,-2,-5)
    ]
  , [new $point(-5,10, 5)
    ,new $point(-5,-2, 5)
    ,new $point(-5,-2,-5)
    ]
  ]
  objectList.forEach(function(a){
    var newbox = new THREE.BoxGeometry(a.size.x,a.size.y,a.size.z)
    var newcube = new THREE.Mesh(newbox, material)
    newcube.position.x = a.pos.x + (a.size.x/2)
    newcube.position.y = a.pos.y + a.size.y
    newcube.position.z = a.pos.z + (a.size.z/2)
    scene.add(newcube)
  })
  planeList.forEach(function(a){
    var newplane = new THREE.Geometry()
    a.forEach(function(b){
      newplane.vertices.push(new THREE.Vector3(b.x,b.y,b.z))
    })
    newplane.faces.push(new THREE.Face3(0,1,2))
    newplane.computeFaceNormals()
    var newmesh = new THREE.Mesh(newplane, new THREE.MeshNormalMaterial())
    scene.add(newmesh)
  })
}

var axisHelper = new THREE.AxisHelper(5)
scene.add(axisHelper)

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
  if(sock.readyState == sock.OPEN){
    sock.send("Ready")
    document.getElementById("connect").style.display = 'none'
    document.getElementById("ready").style.display = 'none'
  }
}
document.getElementById("connect").onclick = function(){
  if(sock.readyState == sock.OPEN){
    sock.send("asdf")
  }
}

document.onkeydown = keyDownHandler
document.onkeyup = keyUpHandler

var keystates = {87: false,65: false, 83: false,68: false,32: false}

var currentMouse = {x: 0, y: 0}
ple = document.getElementsByTagName("canvas")[0]
ple.requestPointerLock = 
  ple.requestPointerLock || 
  ple.mozRequestPointerLock || 
  ple.webkitRequestPointerLock;
ple.onclick = function(){
  ple.requestPointerLock()
  ple.addEventListener("mousemove", mouseMoveHandler, false);
}
ple.addEventListener("pointerlockchange", changeCall, false);
ple.addEventListener("mozpointerlockchange", changeCall, false);

function changeCall(e){
  console.log("change")
}
delta = {}
function mouseMoveHandler(e){
  delta.x = e.movementX || e.mozMovementX || 0
  delta.y = e.movementY || e.mozMovementY || 0
  //console.log(delta.x)
  console.log(e)
  if(sock.readyState == sock.OPEN && document.pointerLockElement === ple || document.mozPointerLockElement === ple){
    console.log("look " + (0-delta.x) + " " + (0-delta.y))
    sock.send("look " + (0-delta.x) + " " + (0-delta.y))
  }
}

function keyDownHandler(e){
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
      case 32:
        sock.send("+jump")
        break;
    }
  }
}
function keyUpHandler(e){
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
    case 32:
      sock.send("-jump")
      break;
  }
}
function parseWorld(worldPlaneList){
  list = worldPlaneList.split(" ")
  parsed = list.map(parseFloat)
}
