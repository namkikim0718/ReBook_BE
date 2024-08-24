package com.be.rebook.chat.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.be.rebook.chat.entity.ChatRoom;

import java.lang.Long;
import java.util.Optional;

public interface ChatRoomRepository extends JpaRepository<ChatRoom, Long> {

    Optional<ChatRoom> findChatRoomById(Long id);
}
