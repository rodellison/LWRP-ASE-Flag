using System;
using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

public class RotateAroundObject : MonoBehaviour
{
    public float speed;
    public bool rotateAroundCenter = false;
    public GameObject objectToRotateAround;
    public GameObject objectToLookAt;
    public bool lookAtObject = false;
    private bool objectToLookAtNotNull = false;
    public bool OscillateVertical = false;
    public float OscillateAmount;
    private float minY = 0;
    private float maxY = 0;


    private Vector3 rotateCenter;
    
    // Update is called once per frame
    void Start()
    {
        rotateCenter = new Vector3(0,0,0);
        if (!rotateAroundCenter)
        {
            if (objectToRotateAround != null)
            {
                float currentY = transform.localPosition.y;
                Vector3 objectToRotateAroundPos = objectToRotateAround.transform.localPosition;
                //keep the y (height) to be that of this objects transform, but set the center x/z to that of the objectToRotateAround
                transform.localPosition = new Vector3(objectToRotateAroundPos.x, currentY, objectToRotateAroundPos.z);
            }
       }
        if (lookAtObject && objectToLookAt != null)
            objectToLookAtNotNull = true;

        if (OscillateVertical)
        {
            minY = transform.localPosition.y - OscillateAmount;
            maxY = transform.localPosition.y + OscillateAmount;
        }
        
    }
    void Update()
    {
        transform.Rotate(0, speed * Time.deltaTime, 0);
        if (objectToLookAtNotNull)
            Camera.main.transform.LookAt(objectToLookAt.transform);
            transform.position = new Vector3(transform.position.x, Mathf.SmoothStep(minY, maxY, Mathf.PingPong(Time.time * 0.1f ,1)),  transform.position.z);
  
        
    }
}
