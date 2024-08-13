package com.be.rebook.chat.service;

import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.stereotype.Service;

@Service
public class ChatRoomService {

    private final RedisMessageListenerContainer redisMessageListener;
    private final RedisSubscriber redisSubscriber;

    public ChatRoomService(RedisMessageListenerContainer redisMessageListener,
                           RedisSubscriber redisSubscriber) {
        this.redisMessageListener = redisMessageListener;
        this.redisSubscriber = redisSubscriber;
    }

    public void subscribeRoom(ChannelTopic topic) {
        redisMessageListener.addMessageListener(redisSubscriber, topic);
    }
}
