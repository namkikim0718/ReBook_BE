package com.be.rebook.chat.controller; 

import com.corundumstudio.socketio.SocketIOServer;
import com.corundumstudio.socketio.annotation.OnConnect;
import com.corundumstudio.socketio.annotation.OnDisconnect;
import com.corundumstudio.socketio.annotation.OnEvent;
import org.springframework.stereotype.Component;

@Component
public class SocketIOController {

    private final SocketIOServer server;

    public SocketIOController(SocketIOServer server) {
        this.server = server;
    }

    @OnConnect
    public void onConnect(com.corundumstudio.socketio.SocketIOClient client) {
        System.out.println("Client connected: " + client.getSessionId());
    }

    @OnDisconnect
    public void onDisconnect(com.corundumstudio.socketio.SocketIOClient client) {
        System.out.println("Client disconnected: " + client.getSessionId());
    }

    @OnEvent("send_message")
    public void onMessage(com.corundumstudio.socketio.SocketIOClient client, String message) {
        System.out.println("Received message: " + message);
        // Broadcast the message to all connected clients
        server.getBroadcastOperations().sendEvent("receive_message", message);
    }
}