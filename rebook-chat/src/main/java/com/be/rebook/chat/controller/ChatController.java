package com.be.rebook.chat.controller;

import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.web.bind.annotation.RestController;

import com.be.rebook.chat.dto.ChatMessage;
import com.be.rebook.chat.service.RedisPublisher;

@RestController
public class ChatController {

    private final RedisPublisher redisPublisher;

    public ChatController(RedisPublisher redisPublisher) {
        this.redisPublisher = redisPublisher;
    }

    @MessageMapping("/sendMessage")
    public void sendMessage(ChatMessage message) {
        // 메시지를 Redis로 퍼블리시
        redisPublisher.publish(message);
    }
}
