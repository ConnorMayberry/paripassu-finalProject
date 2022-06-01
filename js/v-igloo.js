function createIgloos(count) {
    let igloos = []
    for (var i = 0; i < count; i++){
        let baseSize = 10 + 0.5 * noise(i);
        let igloo = new LiveObject(undefined, {
            size: new THREE.Vector3(baseSize, 1, 1)
        })
        let r = 55 + 30 * noise(i * 40)
        let theta = 360 * noise(i)
        console.log("r: ", r, " theta: ", theta)

        let height = baseSize

        let basePosition = new THREE.Vector3(1,1,1)
        basePosition.setToCylindrical(r, theta, height/2)
        igloo.position.x = basePosition; //setToCylindrical(r, theta, baseSize/2)

        let chimney = new THREE.Vector3(1,1,1)
        chimney.setToCylindrical(r, theta, height*1.7)
        chimney.x += 3
        chimney.y -= 2
        chimney.z -= 3
        igloo.chimneyPosition = chimney

        let door = new THREE.Vector3(1,1,1)
        door.setToCylindrical(r, theta, height/4)
        door.x += 10
        door.y -= 1
        door.z -= 0.1
        igloo.doorPosition = door

        igloo.lookAt(0,1,0)

        console.log("igloo: ", igloo)
        igloos.push(igloo)
    }
    return igloos;
}
