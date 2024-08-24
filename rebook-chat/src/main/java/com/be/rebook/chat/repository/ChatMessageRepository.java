package com.be.rebook.chat.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import com.be.rebook.chat.entity.ChatMessage;

import io.lettuce.core.dynamic.annotation.Param;

public interface ChatMessageRepository extends JpaRepository<ChatMessage, Long> {

    @Query("SELECT COUNT(m) FROM ChatMessage m WHERE m.chatRoom.id = :chatRoomId AND m.isRead = false AND m.senderUsername != :username")
    Integer countUnreadMessagesByChatRoomIdAndNotSenderUsername(@Param("chatRoomId") Long chatRoomId,
            @Param("username") String username);
}
