using System;
﻿using System.Collections;
using System.Collections.Generic;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using UnityEngine;

public class Interface : MonoBehaviour
{
  private Thread thread;

  public Vector3 position = new Vector3(0, 0, 0);
  public Vector3 velocity = new Vector3(0, 0, 0);
  public Vector3 euler = new Vector3(0, 0, 0);
  public Quaternion rotation = Quaternion.Euler(new Vector3(0, 0, 0));
  public Int32 index = -1;

  // Setup a mutex because...?
  private static Mutex mut = new Mutex();

  // Start is called before the first frame update
  void Start()
  {
    // Setup thread to receive data from haskell binary.
    thread = new Thread(new ThreadStart(Receive));
    thread.IsBackground = true;
    thread.Start();
  }

  private void OnDestroy() {
    thread.Abort();
  }

  // Update is called once per frame
  void LateUpdate() {
    transform.position = Vector3.Lerp(transform.position, position, 0.1f);
    transform.rotation = Quaternion.Slerp(transform.rotation, rotation, 0.1f);
  }

  private double ToDegrees(double radians)
  {
    return 180.0 * radians / 3.14;
  }


  private void Receive() {
    // This constructor arbitrarily assigns the local port number.
    UdpClient client = new UdpClient(8081);
    //IPEndPoint object will allow us to read datagrams sent from any source.
    IPEndPoint RemoteIpEndPoint = new IPEndPoint(IPAddress.Any, 8081);
    // Flush once.
    client.Receive(ref RemoteIpEndPoint);

    Int32 header_size = 4;

    while (true) {
      try {
        // Blocks until a message returns on this socket from a remote host.
        Byte[] bytes = client.Receive(ref RemoteIpEndPoint);
        // If the incoming message index is ahead of our current index then go ahead and update
        // the position and rotation, otherwise ignore it since its late.
        Int32 received_index = System.BitConverter.ToInt32(bytes, 0);

        if (received_index == 0 && index > 0) {
          index = -1;
        }

        if (received_index > index) {
          mut.WaitOne();
          index = received_index;
          position = new Vector3(
            (float)System.BitConverter.ToDouble(bytes, 8 + header_size), // x
            -(float)System.BitConverter.ToDouble(bytes, 16 + header_size), // z
            (float)System.BitConverter.ToDouble(bytes, 0 + header_size) // y
          );
          velocity = new Vector3(
            (float)System.BitConverter.ToDouble(bytes, 24 + header_size),
            (float)System.BitConverter.ToDouble(bytes, 32 + header_size),
            (float)System.BitConverter.ToDouble(bytes, 40 + header_size)
          );
          euler = new Vector3(
            (float)ToDegrees(-System.BitConverter.ToDouble(bytes, 56 + header_size)), // pitch
            (float)ToDegrees(System.BitConverter.ToDouble(bytes, 64 + header_size)), // yaw
            (float)ToDegrees(-System.BitConverter.ToDouble(bytes, 48 + header_size))  // roll
          );
          rotation = Quaternion.Euler(euler);
          mut.ReleaseMutex();
        } else {
          Debug.Log("Received message index out of order.");
          Debug.Log(index);
          Debug.Log(received_index);
          Debug.Log("....................................");
        }
      }
      catch (Exception e) {
        Debug.Log("Failed to read from socket");
        Debug.Log(e.ToString());
      }
    }
  }

}
