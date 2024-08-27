package com.be.rebook.chat.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.be.rebook.chat.entity.ChatRoom;

import java.lang.Long;
import java.util.List;
import java.util.Optional;

public interface ChatRoomRepository extends JpaRepository<ChatRoom, Long> {

    Optional<ChatRoom> findChatRoomById(Long id);

    @Query("SELECT cr FROM ChatRoom cr WHERE cr.buyerUsername = :memberUsername OR cr.sellerUsername = :memberUsername")
    List<ChatRoom> findChatRoomsByMemberUsername(@Param("memberUsername") String username);

    @Query("SELECT cr FROM ChatRoom cr WHERE cr.sellerUsername = :sellerUsername AND cr.buyerUsername = :buyerUsername AND cr.productId = :productId")
    Optional<ChatRoom> findChatRoomBySellerUsernameAndBuyerUsernameAndProductId(
            @Param("sellerUsername") String sellerUsername,
            @Param("buyerUsername") String buyerUsername,
            @Param("productId") Long productId);
}
