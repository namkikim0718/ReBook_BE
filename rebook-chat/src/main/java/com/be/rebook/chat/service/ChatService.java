package com.be.rebook.chat.service;

import java.nio.channels.Channel;

import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.stereotype.Service;

import com.be.rebook.chat.dto.ChatMessage;

import jakarta.transaction.Transactional;

@Service
public class ChatService {

    private final ChannelTopic channelTopic;

    private final RedisTemplate redisTemplate;

    public ChatService(ChannelTopic channelTopic, RedisTemplate redisTemplate) {
        this.channelTopic = channelTopic;
        this.redisTemplate = redisTemplate;
    }

    @Transactional
    public void sendMessage(ChatMessage chatMessage) {
        // ChatRoom chatRoom =
        // chatRoomRepository.findById(chatMessageRequest.getRoomId()).orElseThrow(ChatRoomNotFoundException::new);

        // 채팅 생성 및 저장
        // ChatMessage chatMessage = ChatMessage.builder()
        // .chatRoom(chatRoom)
        // .user(user)
        // .message(chatMessageRequest.getMessage())
        // .build();

        // chatMessageRepository.save(chatMessage);
        String topic = channelTopic.getTopic();

        // ChatMessageRequest에 유저정보, 현재시간 저장
        // chatMessageRequest.setNickName(user.getNickname());
        // chatMessageRequest.setUserId(user.getId());

        redisTemplate.convertAndSend(topic, chatMessage);
    }

}
