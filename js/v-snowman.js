function createSnowmen(count) {
    let snowmen = []
    for (var i = 0; i < count; i++){
        let baseSize = 0.6 + 0.5*noise(i);
        let middleSize = 2/3*baseSize;
        let topSize = 2/3*middleSize
        let snowman = new LiveObject(undefined, {
            size: new THREE.Vector3(baseSize, middleSize, topSize)
        })
        let r = 20 + 10*noise(i*40)
        let theta = 360*noise(i)
        console.log("r: ", r, " theta: ", theta)

        let height = baseSize

        let basePosition = new THREE.Vector3(1,1,1)
        basePosition.setToCylindrical(r, theta, height)
        snowman.position.x = basePosition; //setToCylindrical(r, theta, baseSize/2)
        
        let middlePosition = new THREE.Vector3(1,1,1)
        middlePosition.setToCylindrical(r, theta, height*2.2)
        snowman.position.y = middlePosition

        let topPosition = new THREE.Vector3(1,1,1)
        topPosition.setToCylindrical(r, theta, height*3)
        snowman.position.z = topPosition

        let leftEye = new THREE.Vector3(1,1,1)
        leftEye.setToCylindrical(r, theta, height*3)
        leftEye.x -= 0.25
        leftEye.y += 0.1
        leftEye.z -= 0.1
        snowman.leftEyePosition = leftEye

        let rightEye = new THREE.Vector3(1,1,1)
        rightEye.setToCylindrical(r, theta, height*3)
        rightEye.x -= 0.25
        rightEye.y += 0.1
        rightEye.z += 0.1
        snowman.rightEyePosition = rightEye

        let nose = new THREE.Vector3(1,1,1)
        nose.setToCylindrical(r, theta, height*3)
        nose.x -= 0.25
        snowman.nosePosition = nose
        
        snowman.lookAt(0,1,0)

        console.log("snowman: ", snowman)
        snowmen.push(snowman)    
    }
    return snowmen;
}