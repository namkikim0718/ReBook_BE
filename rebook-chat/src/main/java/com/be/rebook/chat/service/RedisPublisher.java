package com.be.rebook.chat.service;

import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.be.rebook.chat.dto.ChatMessage;

@Service
public class RedisPublisher {

    private final RedisTemplate<String, Object> redisTemplate;

    public RedisPublisher(RedisTemplate<String, Object> redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    public void publish(ChatMessage message) {
        redisTemplate.convertAndSend("chat/" + message.getRoomId(), message);
    }
}
